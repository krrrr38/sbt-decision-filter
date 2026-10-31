val Scala212 = "2.12.21"
// sbt 2.x plugins must be compiled with the Scala 3 version sbt 2 itself uses.
val Scala3Sbt2 = "3.8.4"

val Sbt1Version = "1.10.0"
val Sbt2Version = "2.0.0"

val ScalatestVersion = "3.2.20"
val ScalametaVersion = "4.17.4"

ThisBuild / organization := "com.krrrr38"
ThisBuild / homepage := Some(url("https://github.com/krrrr38/sbt-decision-filter"))
ThisBuild / licenses := Seq("MIT" -> url("https://opensource.org/licenses/MIT"))
ThisBuild / developers := List(
  Developer("krrrr38", "Ken Kaizu", "k.kaizu38@gmail.com", url("https://github.com/krrrr38"))
)
ThisBuild / scmInfo := Some(
  ScmInfo(
    url("https://github.com/krrrr38/sbt-decision-filter"),
    "scm:git@github.com:krrrr38/sbt-decision-filter.git"
  )
)
ThisBuild / versionScheme := Some("early-semver")

lazy val sbtPluginSettings = Seq(
  scalaVersion := Scala212,
  crossScalaVersions := Seq(Scala212, Scala3Sbt2),
  pluginCrossBuild / sbtVersion := {
    scalaBinaryVersion.value match {
      case "2.12" => Sbt1Version
      case _      => Sbt2Version
    }
  },
  scalacOptions ++= Seq("-deprecation", "-feature", "-unchecked", "-encoding", "utf8") ++
    (if (scalaBinaryVersion.value == "2.12") Seq("-release", "11") else Nil),
  libraryDependencies += "org.scalatest" %% "scalatest" % ScalatestVersion % Test
)

lazy val root = (project in file("."))
  .aggregate(sbtCore, sbtPluginJev, sbtPluginOpenAiDecision)
  .settings(
    name := "sbt-decision-filter-root",
    publish / skip := true,
    crossScalaVersions := Nil
  )

lazy val sbtCore = (project in file("modules/sbt-core"))
  .enablePlugins(SbtPlugin)
  .settings(sbtPluginSettings)
  .settings(
    name := "sbt-decision-filter-core",
    libraryDependencies += "org.scalameta" %% "parsers" % ScalametaVersion
  )

lazy val sbtPluginJev = (project in file("modules/sbt-plugin-jev"))
  .enablePlugins(SbtPlugin)
  .dependsOn(sbtCore % "compile->compile;test->test")
  .settings(sbtPluginSettings)
  .settings(
    name := "sbt-decision-filter-jev",
    scriptedLaunchOpts ++= Seq("-Xmx1024M", s"-Dplugin.version=${version.value}"),
    scriptedBufferLog := false,
    scriptedDependencies := {
      scriptedDependencies.value
      (sbtCore / publishLocal).value
    }
  )

lazy val sbtPluginOpenAiDecision = (project in file("modules/sbt-plugin-openai-decision"))
  .enablePlugins(SbtPlugin)
  .dependsOn(sbtCore % "compile->compile;test->test")
  .settings(sbtPluginSettings)
  .settings(
    name := "sbt-decision-filter-openai-decision"
  )
