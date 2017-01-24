organization := "uk.co.turingatemyhamster"

name := "numerics"

version := "0.0.1-SNAPSHOT"

scalaVersion := "2.11.8"

scalaOrganization := "org.typelevel"

scalacOptions ++= Seq(
  "-deprecation",
  "-encoding",
  "UTF-8",
  "-feature",
  "-language:existentials",
  "-language:experimental.macros",
  "-language:higherKinds",
  "-language:implicitConversions",
  "-unchecked",
  "-Xfatal-warnings",
  "-Xfuture",
  "-Xlint",
  "-Yliteral-types",
  "-Yno-adapted-args",
  "-Ywarn-numeric-widen",
  "-Ywarn-unused-import",
  "-Ywarn-value-discard"
)

libraryDependencies += "eu.timepit" %% "singleton-ops" % "0.0.3-SNAPSHOT"


