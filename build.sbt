// See README.md for license details.

ThisBuild / scalaVersion := "2.13.12"
ThisBuild / version := "0.1.0"
ThisBuild / organization := "com.github.codingplatelets"

val chiselVersion = "6.2.0"
lazy val commonChiselSettings = Seq(
  libraryDependencies ++= Seq(
    "org.chipsalliance" %% "chisel" % chiselVersion,
    "edu.berkeley.cs" %% "chiseltest" % "6.0.0",
    // "edu.berkeley.cs" %% "hardfloat" % "1.5.1-SNAPSHOT"
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
    // fork := true,
    // javaOptions += "-Xmx50G",
    commonChiselSettings
  )

lazy val fputil = Project("fputil", file("depencies/fputil/src"))
  .settings(
    name := "fputil",
    commonChiselSettings
  )
  .settings(
    Compile / scalaSource := baseDirectory.value / "main" / "scala",
    Compile / resourceDirectory := baseDirectory.value / "main" / "resources"
  )
