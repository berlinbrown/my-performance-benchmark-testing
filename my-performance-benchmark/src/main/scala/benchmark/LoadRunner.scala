package benchmark

import java.net.URI
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.time.{Duration, Instant}
import java.util.concurrent.{ConcurrentLinkedQueue, Executors, TimeUnit, ThreadLocalRandom}
import java.util.concurrent.atomic.AtomicLong
import scala.jdk.CollectionConverters.*
import scala.util.control.NonFatal
import org.slf4j.LoggerFactory

final class HttpStatusException(status: Int, method: String, target: URI)
    extends RuntimeException(s"HTTP $status returned for $method $target")

/** Captures the real JDK HttpClient handler callback stack for a 503 response. */
final class DiagnosticBodyHandler extends HttpResponse.BodyHandler[Array[Byte]]:
  @volatile private var captured503Stack: Array[StackTraceElement] = Array.empty

  override def apply(info: HttpResponse.ResponseInfo): HttpResponse.BodySubscriber[Array[Byte]] =
    if info.statusCode() == 503 then
      captured503Stack = Thread.currentThread().getStackTrace.drop(1)
    HttpResponse.BodySubscribers.ofByteArray()

  def stackFor503: Array[StackTraceElement] = captured503Stack

final class LoadRunner(config: TestConfig):
  private val log = LoggerFactory.getLogger(getClass)
  private val client = HttpClient.newBuilder().connectTimeout(Duration.ofSeconds(config.requestTimeoutInSecs)).build()

  def run(): Summary =
    val requestResults = ConcurrentLinkedQueue[RequestResult]()
    val userResults = ConcurrentLinkedQueue[UserResult]()
    val overallRequestNumber = AtomicLong(0)
    val pool = Executors.newFixedThreadPool(config.numberThreadUsers)
    val start = Instant.now()
    val deadline = System.nanoTime() + TimeUnit.SECONDS.toNanos(config.durationInSecs)
    log.info("Starting test: users={}, duration={}s, host={}", config.numberThreadUsers, config.durationInSecs, config.host)
    (0 until config.numberThreadUsers).foreach { userId =>
      pool.submit(new Runnable:
        override def run(): Unit = runUser(userId, deadline, overallRequestNumber, requestResults, userResults)
      )
    }
    pool.shutdown()
    pool.awaitTermination(config.durationInSecs + config.requestTimeoutInSecs + 10, TimeUnit.SECONDS)
    if !pool.isTerminated then
      log.warn("Workers did not stop in time; interrupting them")
      pool.shutdownNow()
      pool.awaitTermination(5, TimeUnit.SECONDS)
    val summary = Summary.build(start, Instant.now(), requestResults.asScala.toVector, userResults.asScala.toVector)
    log.info("Test finished: requests={}, responses={}, unexpected={}, rate={}/sec", summary.requests, summary.responses,
      summary.unexpectedResponses, f"${summary.requestRate}%.1f")
    summary

  private def runUser(id: Int, deadline: Long, overallRequestNumber: AtomicLong,
      results: ConcurrentLinkedQueue[RequestResult], users: ConcurrentLinkedQueue[UserResult]): Unit =
    val started = System.nanoTime()
    var failed = false
    var userRequestNumber = 0L
    try
      while System.nanoTime() < deadline && !Thread.currentThread().isInterrupted do
        config.scenarios.foreach { scenario =>
          if System.nanoTime() < deadline then
            sleep(scenario.delayBeforeReq)
            if System.nanoTime() < deadline then
              userRequestNumber += 1
              val overallNumber = overallRequestNumber.incrementAndGet()
              val result = request(id, userRequestNumber, overallNumber, scenario)
              results.add(result)
              failed ||= !result.expected
              sleep(scenario.delayAfterReq)
        }
    catch
      case _: InterruptedException => Thread.currentThread().interrupt()
      case NonFatal(e) => failed = true; log.error(s"Virtual user $id stopped unexpectedly", e)
    finally users.add(UserResult("0", nanosToMs(System.nanoTime() - started), failed))

  private def request(userId: Int, userRequestNumber: Long, overallRequestNumber: Long,
      scenario: Scenario): RequestResult =
    val target = URI.create(config.host.stripSuffix("/") + "/" + scenario.uri.stripPrefix("/"))
    val body = scenario.body.map(HttpRequest.BodyPublishers.ofString).getOrElse(HttpRequest.BodyPublishers.noBody())
    val builder = HttpRequest.newBuilder(target).timeout(Duration.ofSeconds(config.requestTimeoutInSecs)).method(scenario.method, body)
    scenario.headers.foreach((name, value) => builder.header(name, value))
    val started = System.nanoTime()
    val thread = Thread.currentThread()
    val responseHandler = DiagnosticBodyHandler()
    try
      log.info("REQUEST user={} threadId={} thread={} userRequest={} totalRequest={} method={} url={}",
        userId, thread.threadId(), thread.getName, userRequestNumber, overallRequestNumber, scenario.method, target)
      val response = client.send(builder.build(), responseHandler)
      val elapsed = nanosToMs(System.nanoTime() - started)
      val expected = response.statusCode() == scenario.httpCodeExpected
      val message = "RESPONSE user={} threadId={} thread={} userRequest={} totalRequest={} responseTimeMs={} status={} expectedStatus={} expectedResponseMet={} bytes={}"
      if response.statusCode() == 503 then
        val bodyPreview = printableBodyPreview(response.body())
        val headers = response.headers().map().toString
        val handlerException = HttpStatusException(response.statusCode(), scenario.method, target)
        if responseHandler.stackFor503.nonEmpty then handlerException.setStackTrace(responseHandler.stackFor503)
        log.error(
          """\n================================================================================
HTTP 503 SERVICE UNAVAILABLE
user={} threadId={} thread={} userRequest={} totalRequest={}
method={} url={} responseTimeMs={} expectedStatus={} expectedResponseMet={}
responseHeaders={}
responseBodyPreview={}
JDK HttpClient BodyHandler callback stack follows (the remote server stack is not available):
================================================================================""",
          userId, thread.threadId(), thread.getName, userRequestNumber, overallRequestNumber,
          scenario.method, target, elapsed, scenario.httpCodeExpected, expected, headers, bodyPreview,
          handlerException
        )
      else if expected then
        log.info(message, userId, thread.threadId(), thread.getName, userRequestNumber, overallRequestNumber,
          elapsed, response.statusCode(), scenario.httpCodeExpected, true, response.body().length)
      else
        log.warn(message, userId, thread.threadId(), thread.getName, userRequestNumber, overallRequestNumber,
          elapsed, response.statusCode(), scenario.httpCodeExpected, false, response.body().length)
      RequestResult(Some(response.statusCode()), elapsed, response.body().length, expected)
    catch
      case NonFatal(e) =>
        val elapsed = nanosToMs(System.nanoTime() - started)
        log.error("""\n================================================================================
HTTP REQUEST FAILED
user={} threadId={} thread={} userRequest={} totalRequest={}
responseTimeMs={} status=NO_RESPONSE expectedStatus={} expectedResponseMet=false
Full exception stack follows:
================================================================================""",
          userId, thread.threadId(), thread.getName, userRequestNumber, overallRequestNumber,
          elapsed, scenario.httpCodeExpected, e)
        RequestResult(None, elapsed, 0, expected = false)

  private def sleep(delay: Delay): Unit =
    val millis = delay.`type` match
      case "random" if delay.uptoInMs > 0 => ThreadLocalRandom.current().nextLong(delay.uptoInMs + 1)
      case "fixed" => delay.uptoInMs
      case _ => 0L
    if millis > 0 then Thread.sleep(millis)

  private def printableBodyPreview(bytes: Array[Byte]): String =
    val limit = math.min(bytes.length, 2048)
    val text = String(bytes, 0, limit, java.nio.charset.StandardCharsets.UTF_8)
      .replaceAll("[\\p{Cntrl}&&[^\\r\\n\\t]]", "?")
    if bytes.length > limit then s"$text... [truncated; ${bytes.length} bytes total]" else text

  private def nanosToMs(nanos: Long): Long = TimeUnit.NANOSECONDS.toMillis(nanos)
