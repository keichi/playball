name := "playboy"

version := "1.0-SNAPSHOT"

libraryDependencies ++= Seq(
  jdbc,
  cache,
  "com.typesafe.slick" %% "slick" % "2.0.0",
  "com.typesafe.play" %% "play-slick" % "0.6.0.1",
  "org.scala-lang" % "scala-reflect" % "2.10.2",
  "joda-time" % "joda-time" % "2.3",
  "org.joda" % "joda-convert" % "1.5",
  "com.github.tototoshi" %% "slick-joda-mapper" % "1.1.0"
)     

play.Project.playScalaSettings
