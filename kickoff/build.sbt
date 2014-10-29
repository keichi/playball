name := "playboy-kickoff"

version := "0.1"

scalaVersion := "2.10.3"

libraryDependencies ++= Seq(
  "org.scalatra.scalate" %% "scalate-core" % "1.7.0",
  "org.scala-lang" % "scala-reflect" % scalaVersion.value
)
