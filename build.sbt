name := "adventofcode"

version := "1.0"

scalaVersion := "2.11.7"

resolvers ++=  Seq(
  "Typesafe repository" at "http://repo.typesafe.com/typesafe/releases/"
)

libraryDependencies ++= Seq (
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4",
  "org.scalatest" %% "scalatest" % "2.2.5" % "test" excludeAll(
    ExclusionRule(organization = "org.scala-lang")
    )
)

lazy val compileScalastyle = taskKey[Unit]("compileScalastyle")

compileScalastyle := org.scalastyle.sbt.ScalastylePlugin.scalastyle.in(Compile).toTask("").value

(compile in Compile) <<= (compile in Compile) dependsOn (compileScalastyle, cpd)

import de.johoop.cpd4sbt.CopyPasteDetector._
import de.johoop.cpd4sbt._

cpdSettings

cpdMinimumTokens := 100

coverageMinimum := 80

coverageFailOnMinimum := true
