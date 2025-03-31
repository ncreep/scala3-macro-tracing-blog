import sbt._

ThisBuild / scalaVersion := "3.6.4"

ThisBuild / scalacOptions ++= List(
  "-deprecation",
  "-experimental")

// Enable to debug macros
ThisBuild / scalacOptions += "-Xprint:postInlining"
// ThisBuild / scalacOptions += "-Xprint:erasure"

// it's a good idea to enable extra checks, but it makes the output from SBT
// very noisy
// ThisBuild / scalacOptions += "-Xcheck-macros"
// ThisBuild / scalacOptions += "-Ycheck:all"

// ThisBuild / scalacOptions += "-explain"

Compile / console / scalacOptions -= "-Xprint:postInlining"

ThisBuild / initialCommands := """
  import ncreep.tracing.*
"""
