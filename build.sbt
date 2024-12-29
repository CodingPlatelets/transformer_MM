// See README.md for license details.

ThisBuild / scalaVersion := "2.13.12"
ThisBuild / version := "0.1.0"
ThisBuild / organization := "com.github.codingplatelets"

val chiselVersion = "6.2.0"
lazy val commonChiselSettings = Seq(
  libraryDependencies ++= Seq(
    "org.chipsalliance" %% "chisel" % chiselVersion,
    "edu.berkeley.cs" %% "chiseltest" % "6.0.0",
    "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.4"
  ),
  resolvers += "huaweiyun".at("https://repo.huaweicloud.com/repository/maven/"),
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
  .dependsOn(fputilNopipe)
  .dependsOn(hardfloat)
  .settings(
    name := "transformer_MM",
    // fork := true,
    // javaOptions += "-Xmx50G",
    commonChiselSettings
  )

lazy val fputil = Project("fputil", file("dependencies/fputil/src"))
  .settings(
    name := "fputil",
    commonChiselSettings
  )
  .settings(
    Compile / scalaSource := baseDirectory.value / "main" / "scala",
    Compile / resourceDirectory := baseDirectory.value / "main" / "resources"
  )
lazy val fputilNopipe = Project("fputilNopipe", file("dependencies/fputil-nopipe/src"))
  .settings(
    name := "fputilNopipe",
    commonChiselSettings
  )
  .settings(
    Compile / scalaSource := baseDirectory.value / "main" / "scala",
    Compile / resourceDirectory := baseDirectory.value / "main" / "resources"
  )
lazy val hardfloat = Project("hardfloat", file("dependencies/hardfloat/hardfloat/src"))
  .settings(
    name := "hardfloat",
    commonChiselSettings
  )
  .settings(
    Compile / scalaSource := baseDirectory.value / "main" / "scala",
    Compile / resourceDirectory := baseDirectory.value / "main" / "resources"
  )
