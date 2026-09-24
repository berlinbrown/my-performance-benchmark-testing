package benchmark

import java.nio.file.{Files, Path}
import java.time.{Duration, Instant, ZoneId}
import java.time.format.DateTimeFormatter

final case class RequestResult(status: Option[Int], elapsedMs: Long, bytes: Long, expected: Boolean,
    completedAt: Instant = Instant.now())
final case class UserResult(name: String, elapsedMs: Long, failed: Boolean)
final case class TimeBucket(startSecond: Long, endSecond: Long, successful: Long, unsuccessful: Long,
    averageResponseMs: Double, medianResponseMs: Double)
final case class Stats(min: Double, max: Double, median: Double, p90: Double, p95: Double, p99: Double)

object Stats:
  def from(values: Seq[Long]): Stats =
    if values.isEmpty then Stats(0, 0, 0, 0, 0, 0)
    else
      val sorted = values.sorted
      def percentile(p: Double): Double = sorted(math.max(0, math.ceil(p * sorted.size).toInt - 1)).toDouble
      Stats(sorted.head, sorted.last, percentile(.5), percentile(.90), percentile(.95), percentile(.99))

final case class Summary(startedAt: Instant, finishedAt: Instant, requests: Long, responses: Long,
    downloadedBytes: Long, requestRate: Double, codes: Map[Int, Long], unexpectedResponses: Long,
    responseTime: Stats, usersCreated: Long, usersCompleted: Long, usersFailed: Long,
    usersByName: Map[String, Long], sessionLength: Stats, timeBuckets: Vector[TimeBucket])

object Summary:
  def build(start: Instant, finish: Instant, requests: Seq[RequestResult], users: Seq[UserResult]): Summary =
    val seconds = math.max(Duration.between(start, finish).toMillis / 1000.0, .001)
    val bucketWidthSeconds = math.max(1L, math.ceil(seconds / 30.0).toLong)
    val bucketCount = math.max(1, math.ceil(seconds / bucketWidthSeconds).toInt)
    val grouped = requests.groupBy { request =>
      val offset = math.max(0L, Duration.between(start, request.completedAt).toMillis)
      math.min(bucketCount - 1, (offset / (bucketWidthSeconds * 1000L)).toInt)
    }
    val buckets = Vector.tabulate(bucketCount) { index =>
      val values = grouped.getOrElse(index, Seq.empty)
      val responses = values.filter(_.status.nonEmpty)
      val successful = responses.count(_.status.exists(code => code >= 200 && code < 300)).toLong
      val unsuccessful = responses.size.toLong - successful
      val latencies = responses.map(_.elapsedMs)
      TimeBucket(index * bucketWidthSeconds, math.min(math.ceil(seconds).toLong, (index + 1) * bucketWidthSeconds),
        successful, unsuccessful, if latencies.isEmpty then 0.0 else latencies.sum.toDouble / latencies.size,
        Stats.from(latencies).median)
    }
    Summary(start, finish, requests.size, requests.count(_.status.nonEmpty), requests.map(_.bytes).sum,
      requests.size / seconds, requests.flatMap(_.status).groupMapReduce(identity)(_ => 1L)(_ + _),
      requests.count(!_.expected), Stats.from(requests.map(_.elapsedMs)), users.size,
      users.count(!_.failed), users.count(_.failed), users.groupMapReduce(_.name)(_ => 1L)(_ + _),
      Stats.from(users.map(_.elapsedMs)), buckets)

object ReportWriter:
  private val time = DateTimeFormatter.ofPattern("HH:mm:ss(XXX)").withZone(ZoneId.systemDefault())
  private def line(label: String, value: String): String = f"$label%-34s ${"." * math.max(1, 64 - label.length)} $value"
  private def number(v: Double): String = if v == v.toLong then v.toLong.toString else f"$v%.1f"
  private def statusBreakdown(summary: Summary): (Long, Double, Long, Double) =
    val successful = summary.codes.iterator.collect { case (code, count) if code >= 200 && code < 300 => count }.sum
    val unsuccessful = summary.codes.iterator.collect { case (code, count) if code < 200 || code >= 300 => count }.sum
    val total = successful + unsuccessful
    def percentage(count: Long): Double = if total == 0 then 0.0 else count * 100.0 / total
    (successful, percentage(successful), unsuccessful, percentage(unsuccessful))
  private def countAndPercentage(count: Long, percentage: Double): String = f"$count%,d ($percentage%.1f%%)"
  private def appendStats(b: StringBuilder, s: Stats): Unit =
    Seq("min" -> s.min, "max" -> s.max, "median" -> s.median, "p90" -> s.p90, "p95" -> s.p95, "p99" -> s.p99)
      .foreach((label, value) => b ++= line(s"  $label:", number(value)) + "\n")

  def text(summary: Summary): String =
    val b = StringBuilder(s"--------------------------------\nSummary report @ ${time.format(summary.finishedAt)}\n--------------------------------\n\n")
    val (successful, successfulPercentage, unsuccessful, unsuccessfulPercentage) = statusBreakdown(summary)
    summary.codes.toSeq.sortBy(_._1).foreach((code, count) => b ++= line(s"http.codes.$code:", count.toString) + "\n")
    b ++= line("http.responses.2xx:", countAndPercentage(successful, successfulPercentage)) + "\n"
    b ++= line("http.responses.non_2xx:", countAndPercentage(unsuccessful, unsuccessfulPercentage)) + "\n"
    b ++= line("http.downloaded_bytes:", summary.downloadedBytes.toString) + "\n"
    b ++= line("http.request_rate:", f"${summary.requestRate}%.1f/sec") + "\n"
    b ++= line("http.requests:", summary.requests.toString) + "\nhttp.response_time (ms):\n"
    appendStats(b, summary.responseTime)
    b ++= line("http.responses:", summary.responses.toString) + "\n"
    b ++= line("http.unexpected_responses:", summary.unexpectedResponses.toString) + "\n"
    b ++= line("vusers.completed:", summary.usersCompleted.toString) + "\n"
    b ++= line("vusers.created:", summary.usersCreated.toString) + "\n"
    summary.usersByName.toSeq.sortBy(_._1).foreach((name, count) => b ++= line(s"vusers.created_by_name.$name:", count.toString) + "\n")
    b ++= line("vusers.failed:", summary.usersFailed.toString) + "\nvusers.session_length (ms):\n"
    appendStats(b, summary.sessionLength)
    b.result()

  private def statusChart(buckets: Vector[TimeBucket]): String =
    val width = 820.0
    val height = 280.0
    val left = 58.0
    val top = 22.0
    val plotWidth = 738.0
    val plotHeight = 205.0
    val maxCount = math.max(1L, buckets.map(b => b.successful + b.unsuccessful).maxOption.getOrElse(0L))
    val slot = plotWidth / math.max(1, buckets.size)
    val barWidth = math.max(2.0, slot * .72)
    val bars = buckets.zipWithIndex.map { (bucket, index) =>
      val x = left + index * slot + (slot - barWidth) / 2
      val successHeight = bucket.successful.toDouble / maxCount * plotHeight
      val errorHeight = bucket.unsuccessful.toDouble / maxCount * plotHeight
      val successY = top + plotHeight - successHeight
      val errorY = successY - errorHeight
      s"""<g><title>${bucket.startSecond}-${bucket.endSecond}s: ${bucket.successful} 2xx, ${bucket.unsuccessful} non-2xx</title>
<rect x="$x" y="$successY" width="$barWidth" height="$successHeight" fill="#16803c"/>
<rect x="$x" y="$errorY" width="$barWidth" height="$errorHeight" fill="#b42318"/></g>"""
    }.mkString("\n")
    val lastSecond = buckets.lastOption.map(_.endSecond).getOrElse(0L)
    s"""<svg class="chart" viewBox="0 0 $width $height" role="img" aria-labelledby="status-chart-title status-chart-desc">
<title id="status-chart-title">HTTP responses over time</title><desc id="status-chart-desc">Stacked response counts per time bucket. Green is 2xx and red is non-2xx.</desc>
<line x1="$left" y1="$top" x2="$left" y2="${top + plotHeight}" class="axis"/><line x1="$left" y1="${top + plotHeight}" x2="${left + plotWidth}" y2="${top + plotHeight}" class="axis"/>
<text x="${left - 8}" y="${top + 5}" class="axis-label" text-anchor="end">$maxCount</text><text x="${left - 8}" y="${top + plotHeight + 5}" class="axis-label" text-anchor="end">0</text>
$bars
<text x="$left" y="${top + plotHeight + 24}" class="axis-label">0s</text><text x="${left + plotWidth}" y="${top + plotHeight + 24}" class="axis-label" text-anchor="end">${lastSecond}s</text>
<g transform="translate($left,265)"><rect width="12" height="12" fill="#16803c"/><text x="18" y="11" class="legend">2xx</text><rect x="75" width="12" height="12" fill="#b42318"/><text x="93" y="11" class="legend">Non-2xx</text></g></svg>"""

  private def latencyChart(buckets: Vector[TimeBucket]): String =
    val width = 820.0
    val height = 280.0
    val left = 58.0
    val top = 22.0
    val plotWidth = 738.0
    val plotHeight = 205.0
    val maxLatency = math.max(1.0, buckets.flatMap(b => Seq(b.averageResponseMs, b.medianResponseMs)).maxOption.getOrElse(0.0))
    val divisor = math.max(1, buckets.size - 1)
    def points(value: TimeBucket => Double): String = buckets.zipWithIndex.map { (bucket, index) =>
      val x = left + index.toDouble / divisor * plotWidth
      val y = top + plotHeight - value(bucket) / maxLatency * plotHeight
      f"$x%.1f,$y%.1f"
    }.mkString(" ")
    val dots = buckets.zipWithIndex.map { (bucket, index) =>
      val x = left + index.toDouble / divisor * plotWidth
      val y = top + plotHeight - bucket.medianResponseMs / maxLatency * plotHeight
      f"<circle cx=\"$x%.1f\" cy=\"$y%.1f\" r=\"2.5\" fill=\"#2563eb\"><title>${bucket.startSecond}-${bucket.endSecond}s: median ${bucket.medianResponseMs}%.1f ms, average ${bucket.averageResponseMs}%.1f ms</title></circle>"
    }.mkString("\n")
    val lastSecond = buckets.lastOption.map(_.endSecond).getOrElse(0L)
    s"""<svg class="chart" viewBox="0 0 $width $height" role="img" aria-labelledby="latency-chart-title latency-chart-desc">
<title id="latency-chart-title">HTTP response time over time</title><desc id="latency-chart-desc">Median and average response time in milliseconds per time bucket.</desc>
<line x1="$left" y1="$top" x2="$left" y2="${top + plotHeight}" class="axis"/><line x1="$left" y1="${top + plotHeight}" x2="${left + plotWidth}" y2="${top + plotHeight}" class="axis"/>
<text x="${left - 8}" y="${top + 5}" class="axis-label" text-anchor="end">${number(maxLatency)} ms</text><text x="${left - 8}" y="${top + plotHeight + 5}" class="axis-label" text-anchor="end">0</text>
<polyline points="${points(_.averageResponseMs)}" fill="none" stroke="#7c3aed" stroke-width="2" stroke-dasharray="6 4"/><polyline points="${points(_.medianResponseMs)}" fill="none" stroke="#2563eb" stroke-width="3"/>$dots
<text x="$left" y="${top + plotHeight + 24}" class="axis-label">0s</text><text x="${left + plotWidth}" y="${top + plotHeight + 24}" class="axis-label" text-anchor="end">${lastSecond}s</text>
<g transform="translate($left,265)"><line x2="14" y1="6" y2="6" stroke="#2563eb" stroke-width="3"/><text x="20" y="11" class="legend">Median</text><line x1="90" x2="104" y1="6" y2="6" stroke="#7c3aed" stroke-width="2" stroke-dasharray="6 4"/><text x="110" y="11" class="legend">Average</text></g></svg>"""

  def html(summary: Summary): String =
    def row(label: String, value: Any, cssClass: String = ""): String =
      val classAttribute = if cssClass.nonEmpty then s" class=\"${escape(cssClass)}\"" else ""
      s"<tr$classAttribute><th>${escape(label)}</th><td>${escape(value.toString)}</td></tr>"
    def statsRows(stats: Stats, unit: String): String =
      Seq("Minimum" -> stats.min, "Maximum" -> stats.max, "Median (p50)" -> stats.median,
        "p90" -> stats.p90, "p95" -> stats.p95, "p99" -> stats.p99)
        .map((label, value) => row(s"$label ($unit)", number(value))).mkString("\n")
    val codeRows = summary.codes.toSeq.sortBy(_._1).map { (code, count) =>
      row(s"HTTP $code", count, if code >= 200 && code < 300 then "" else "http-error")
    }.mkString("\n")
    val (successful, successfulPercentage, unsuccessful, unsuccessfulPercentage) = statusBreakdown(summary)
    val statusClassRows =
      row("2xx responses", countAndPercentage(successful, successfulPercentage)) +
      row("Non-2xx responses", countAndPercentage(unsuccessful, unsuccessfulPercentage), if unsuccessful > 0 then "http-error" else "")
    val userNameRows = summary.usersByName.toSeq.sortBy(_._1)
      .map((name, count) => row(s"Created for scenario $name", count)).mkString("\n")
    s"""<!doctype html><html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1"><title>Performance test summary</title><style>
body{margin:0;background:#f4f7fb;color:#172033;font:15px system-ui,sans-serif}.report{max-width:900px;margin:40px auto}.hero,.section{background:white;border-radius:12px;box-shadow:0 6px 24px #17203316;overflow:hidden;margin-bottom:20px}.hero{padding:28px 34px;background:#172033;color:white}h1{margin:0;font-size:24px}.hero p{margin:8px 0 0;color:#b9c5d9}h2{font-size:17px;margin:0;padding:18px 28px;background:#f8fafc;border-bottom:1px solid #e7ebf2}table{border-collapse:collapse;width:100%}th,td{padding:10px 28px;border-bottom:1px solid #e7ebf2}tr:last-child th,tr:last-child td{border-bottom:0}th{text-align:left;font-weight:500}td{text-align:right;font:600 14px ui-monospace,monospace}.http-error{background:#fff1f2;color:#b42318}.http-error th,.http-error td{font-weight:700}.hint{font-weight:400;color:#637083;font-size:13px}.chart{display:block;width:100%;height:auto;background:white}.axis{stroke:#94a3b8;stroke-width:1}.axis-label,.legend{fill:#526174;font:12px system-ui,sans-serif}@media(max-width:600px){.report{margin:0}.hero,.section{border-radius:0;margin-bottom:10px}th,td{padding:9px 16px}}
</style></head><body><main class="report"><header class="hero"><h1>Performance test summary</h1><p>${escape(time.format(summary.finishedAt))}</p></header>
<section class="section"><h2>HTTP responses over time <span class="hint">stacked counts per time bucket</span></h2>${statusChart(summary.timeBuckets)}</section>
<section class="section"><h2>HTTP response time over time <span class="hint">median and average per time bucket</span></h2>${latencyChart(summary.timeBuckets)}</section>
<section class="section"><h2>HTTP totals</h2><table><tbody>$codeRows$statusClassRows
${row("Downloaded bytes", summary.downloadedBytes)}${row("Request rate", f"${summary.requestRate}%.1f/sec")}${row("Requests", summary.requests)}${row("Responses", summary.responses)}${row("Unexpected responses", summary.unexpectedResponses, if summary.unexpectedResponses > 0 then "http-error" else "")}</tbody></table></section>
<section class="section"><h2>HTTP response latency <span class="hint">time for one HTTP request</span></h2><table><tbody>${statsRows(summary.responseTime, "ms")}</tbody></table></section>
<section class="section"><h2>Virtual users</h2><table><tbody>${row("Created", summary.usersCreated)}${row("Completed", summary.usersCompleted)}${row("Failed", summary.usersFailed)}$userNameRows</tbody></table></section>
<section class="section"><h2>Virtual-user session duration <span class="hint">lifetime of each user, approximately the configured test duration</span></h2><table><tbody>${statsRows(summary.sessionLength, "ms")}</tbody></table></section>
</main></body></html>"""

  private def escape(s: String): String = s.replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;").replace("\"", "&quot;")
  def write(summary: Summary, output: Path): Unit =
    Files.createDirectories(output)
    Files.writeString(output.resolve("summary.txt"), text(summary))
    Files.writeString(output.resolve("summary.html"), html(summary))
