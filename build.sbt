name := "sca"

version := "1.0"

scalaVersion := "2.10.4"

resolvers += Resolver.sonatypeRepo("snapshots")

libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-swing" % "2.10.4",
  "org.scalatest" % "scalatest_2.10" % "2.1.3" % "test",
  "com.nativelibs4java" %% "scalaxy-loops" % "0.3-SNAPSHOT" % "provided"
)
