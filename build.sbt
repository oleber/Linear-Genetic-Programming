name := """linear-genetic-programming"""

version := "1.0"

scalaVersion := "2.11.7"

libraryDependencies ++= Seq(
  "org.specs2" % "specs2-core_2.11" % "3.7"
)

fork in run := true