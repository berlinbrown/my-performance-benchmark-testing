# Scala HTTP performance benchmark

A small Artillery-style load tester. A fixed number of virtual users repeatedly execute every scenario for the configured duration. Requests use Java's `HttpClient`; logging uses SLF4J and rolling Logback files.

## Run

Start the server under test, then run:

```bash
sbt 'run example.json report'
```

This creates `report/summary.txt`, `report/summary.html`, and a rolling `logs/performance-test.log`. The summary includes status-code counts, response bytes, request rate, request latency percentiles, unexpected responses, and virtual-user results.

While a test runs, every request and response is printed. Each entry includes the virtual-user number, JVM thread ID and name, per-user request number, overall request number, HTTP method and URL, response time, actual and expected status, whether the expected response was met, and downloaded bytes. The request numbers are running counters because the duration-based test does not know the final total until it finishes.

The configuration accepts either JSON numbers or numeric strings. See `example.json`. Each scenario requires `uri` and `httpCodeExpected`; optional fields are `name`, `method`, `headers`, `body`, `delayBeforeReq`, and `delayAfterReq`. Delay type may be `random`, `fixed`, or `none`. A random delay is uniformly selected from zero through `uptoInMs` inclusive.

`numberThreadUsers` means concurrent long-lived virtual users, so `vusers.created` equals that value. A virtual user is marked failed if any request fails or returns a status other than `httpCodeExpected`.
