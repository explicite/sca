name := "sca"

version := "1.0"

scalaVersion := "2.11.1"

resolvers += Resolver.sonatypeRepo("snapshots")

libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-library-all" % "2.11.1",
  "org.scalatest" % "scalatest_2.11" % "2.1.7" % "test"
)
