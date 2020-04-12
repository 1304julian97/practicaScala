name := "practicaScala"

version := "0.1"

scalaVersion := "2.12.6"

libraryDependencies ++= Seq("org.scalatest" %% "scalatest" % "3.0.5" % "test",
 "info.cukes" %% "cucumber-scala" % "1.2.6" % Test pomOnly(),
"info.cukes" % "cucumber-java8" % "1.2.5" % "test",
"info.cukes" % "cucumber-junit" % "1.2.5" % "test",
"info.cukes" % "cucumber-picocontainer" % "1.2.5" % "test")
