// See README.md for license details.

ThisBuild / scalaVersion := "2.13.12"
ThisBuild / version := "0.1.0"
ThisBuild / organization := "com.github.codingplatelets"

val chiselVersion = "6.2.0"
lazy val commonChiselSettings = Seq(
  libraryDependencies ++= Seq(
    "org.chipsalliance" %% "chisel" % chiselVersion,
    "edu.berkeley.cs" %% "chiseltest" % "6.0.0"
  ),
  resolvers += "aliyun".at("https://maven.aliyun.com/repository/public"),
  scalacOptions ++= Seq(
    "-language:reflectiveCalls",
    "-deprecation",
    "-feature",
    "-Xcheckinit",
    "-Ymacro-annotations"
  ),
  addCompilerPlugin(("org.chipsalliance" % "chisel-plugin" % chiselVersion).cross(CrossVersion.full))
)

lazy val root = (project in file("."))
  .dependsOn(fputil)
  .settings(
    name := "transformer_MM",
    commonChiselSettings
  )

lazy val fputil = (project in file("fputil/src/main/scala")).settings(
  name := "fputil",
  commonChiselSettings
)

pomExtra := (
  <url>http://chisel.eecs.berkeley.edu/</url>
<licenses>
  <license>
    <name>BSD-style</name>
    <url>http://www.opensource.org/licenses/bsd-license.php</url>
    <distribution>repo</distribution>
  </license>
</licenses>
<scm>
  <url>https://github.com/ucb-bar/chisel-testers2.git</url>
  <connection>scm:git:github.com/ucb-bar/chisel-testers2.git</connection>
</scm>
)
