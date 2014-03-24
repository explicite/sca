name := "sca"

version := "1.0"

libraryDependencies += "org.scalatest" % "scalatest_2.10" % "2.0" % "test"

libraryDependencies += "org.scala-lang" % "scala-swing" % "2.10.3"

libraryDependencies += "org.scala-lang" % "scala-actors" % "2.10.3"

libraryDependencies += "com.nativelibs4java" %% "scalaxy-loops" % "0.3-SNAPSHOT" % "provided"

resolvers += Resolver.sonatypeRepo("snapshots")