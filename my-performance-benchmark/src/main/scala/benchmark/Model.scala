package benchmark

import java.nio.file.{Files, Path}

final case class Delay(`type`: String = "none", uptoInMs: Long = 0):
  require(Set("none", "fixed", "random").contains(`type`), s"unsupported delay type: ${`type`}")
  require(uptoInMs >= 0, "delay must be non-negative")

final case class Scenario(name: String, uri: String, httpCodeExpected: Int, method: String = "GET",
    headers: Map[String, String] = Map.empty, body: Option[String] = None,
    delayBeforeReq: Delay = Delay(), delayAfterReq: Delay = Delay())

final case class TestConfig(durationInSecs: Long, numberThreadUsers: Int, host: String,
    scenarios: Vector[Scenario], requestTimeoutInSecs: Long = 30):
  require(durationInSecs > 0, "durationInSecs must be positive")
  require(numberThreadUsers > 0, "numberThreadUsers must be positive")
  require(scenarios.nonEmpty, "at least one scenario is required")

object TestConfig:
  def load(path: Path): TestConfig = parse(Files.readString(path))

  def parse(json: String): TestConfig =
    val root = ujson.read(json).obj.get("config").map(_.obj).getOrElse(throw IllegalArgumentException("missing config object"))
    def text(value: ujson.Value, field: String): String = value match
      case ujson.Str(v) => v
      case ujson.Num(v) => if v.isWhole then v.toLong.toString else v.toString
      case _ => throw IllegalArgumentException(s"$field must be a string or number")
    def required(field: String): ujson.Value = root.getOrElse(field, throw IllegalArgumentException(s"missing config.$field"))
    def long(field: String, default: Option[Long] = None): Long = root.get(field).map(v => text(v, field).toLong)
      .orElse(default).getOrElse(throw IllegalArgumentException(s"missing config.$field"))
    def delay(value: Option[ujson.Value], field: String): Delay =
      value.map(_.obj).map { d => Delay(d.get("type").map(text(_, s"$field.type")).getOrElse("none"),
        d.get("uptoInMs").map(text(_, s"$field.uptoInMs").toLong).getOrElse(0L)) }.getOrElse(Delay())

    val scenarios = required("scenarios").arr.zipWithIndex.map { case (value, index) =>
      val obj = value.obj
      def req(field: String): String = obj.get(field).map(text(_, s"scenarios[$index].$field"))
        .getOrElse(throw IllegalArgumentException(s"missing scenarios[$index].$field"))
      val headers = obj.get("headers").map(_.obj.iterator.map { case (k, v) => k -> text(v, s"headers.$k") }.toMap).getOrElse(Map.empty)
      Scenario(obj.get("name").map(text(_, "name")).getOrElse(index.toString), req("uri"), req("httpCodeExpected").toInt,
        obj.get("method").map(text(_, "method").toUpperCase).getOrElse("GET"), headers,
        obj.get("body").map(text(_, "body")), delay(obj.get("delayBeforeReq"), "delayBeforeReq"),
        delay(obj.get("delayAfterReq"), "delayAfterReq"))
    }.toVector
    TestConfig(long("durationInSecs"), long("numberThreadUsers").toInt, text(required("host"), "host"),
      scenarios, long("requestTimeoutInSecs", Some(30)))
