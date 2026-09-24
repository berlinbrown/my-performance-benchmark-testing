// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
import benchmark.*
import java.time.Instant
import com.sun.net.httpserver.HttpServer
import java.net.InetSocketAddress

class MySuite extends munit.FunSuite:
  test("configuration accepts string and numeric values"):
    val config = TestConfig.parse("""{"config":{"durationInSecs":"2","numberThreadUsers":3,"host":"http://localhost","scenarios":[{"uri":"/ok","httpCodeExpected":"200","delayBeforeReq":{"type":"random","uptoInMs":"5"}}]}}""")
    assertEquals(config.durationInSecs, 2L)
    assertEquals(config.numberThreadUsers, 3)
    assertEquals(config.scenarios.head.delayBeforeReq.uptoInMs, 5L)

  test("summary computes counts and nearest-rank percentiles"):
    val start = Instant.parse("2026-01-01T00:00:00Z")
    val requests = Seq(RequestResult(Some(200), 1, 7, true), RequestResult(Some(500), 10, 3, false))
    val summary = Summary.build(start, start.plusSeconds(2), requests, Seq(UserResult("0", 2000, true)))
    assertEquals(summary.codes, Map(200 -> 1L, 500 -> 1L))
    assertEquals(summary.downloadedBytes, 10L)
    assertEquals(summary.requestRate, 1.0)
    assertEquals(summary.responseTime.p90, 10.0)
    assertEquals(summary.responseTime.p95, 10.0)
    val html = ReportWriter.html(summary)
    assert(html.contains("HTTP response latency"))
    assert(html.contains("HTTP 200"))
    assert(html.contains("<tr class=\"http-error\"><th>HTTP 500</th>"))
    assert(html.contains("<th>2xx responses</th><td>1 (50.0%)</td>"))
    assert(html.contains("<th>Non-2xx responses</th><td>1 (50.0%)</td>"))
    val reportText = ReportWriter.text(summary)
    assert(reportText.contains("http.responses.2xx:"))
    assert(reportText.contains("1 (50.0%)"))

  test("runner sends concurrent HTTP requests and records responses"):
    val server = HttpServer.create(InetSocketAddress("127.0.0.1", 0), 0)
    server.createContext("/test", exchange =>
      val bytes = "ok".getBytes
      exchange.sendResponseHeaders(200, bytes.length)
      exchange.getResponseBody.write(bytes)
      exchange.close()
    )
    server.start()
    try
      val config = TestConfig(1, 2, s"http://127.0.0.1:${server.getAddress.getPort}",
        Vector(Scenario("test", "/test", 200)))
      val summary = LoadRunner(config).run()
      assert(summary.requests > 0)
      assertEquals(summary.requests, summary.responses)
      assertEquals(summary.codes.keySet, Set(200))
      assertEquals(summary.usersCreated, 2L)
      assertEquals(summary.usersFailed, 0L)
    finally server.stop(0)
