val scala3Version = "3.9.0"

lazy val root = project
  .in(file("."))
  .settings(
    name := "my-performance-benchmark",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    libraryDependencies ++= Seq(
      "com.lihaoyi" %% "ujson" % "4.3.2",
      "org.slf4j" % "slf4j-api" % "2.0.17",
      "ch.qos.logback" % "logback-classic" % "1.5.18",
      "org.scalameta" %% "munit" % "1.3.6" % Test
    )
  )
