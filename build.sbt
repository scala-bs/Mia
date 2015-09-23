name := """meiern"""

version := "1.0"

scalaVersion := "2.11.6"

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-actor" % "2.3.13",
  "com.typesafe.akka" %% "akka-remote" % "2.3.13",
  "org.scalatest" %% "scalatest" % "2.2.4" % "test",
  "com.typesafe.akka" % "akka-testkit_2.11" % "2.3.14"
)
