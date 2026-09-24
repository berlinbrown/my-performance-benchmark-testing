import benchmark.{LoadRunner, ReportWriter, TestConfig}
import java.nio.file.Path
import org.slf4j.LoggerFactory

@main def runBenchmark(configFile: String, outputDirectory: String = "report"): Unit =
  val log = LoggerFactory.getLogger("benchmark.Main")
  try
    val config = TestConfig.load(Path.of(configFile))
    val summary = LoadRunner(config).run()
    val output = Path.of(outputDirectory).toAbsolutePath
    ReportWriter.write(summary, output)
    println(ReportWriter.text(summary))
    log.info("Reports written to {}", output)
  catch
    case e: Exception =>
      log.error("Performance test failed", e)
      sys.exit(1)
