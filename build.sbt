name := "playball-app"

version := "1.0-SNAPSHOT"

scalacOptions ++= Seq("-feature", "-deprecation")

libraryDependencies ++= Seq(
  jdbc,
  cache,
  "com.typesafe.slick" %% "slick" % "2.0.3",
  "com.typesafe.play" %% "play-slick" % "0.6.1",
  "org.scala-lang" % "scala-reflect" % "2.10.3",
  "joda-time" % "joda-time" % "2.3",
  "org.joda" % "joda-convert" % "1.5",
  "com.github.tototoshi" %% "slick-joda-mapper" % "1.1.0"
)     

play.Project.playScalaSettings

addCompilerPlugin("org.scalamacros" % "paradise" % "2.0.1" cross CrossVersion.full)

lazy val app = project.in(file("."))
    .aggregate(libs, macros)
    .dependsOn(libs, macros)

lazy val libs = project

lazy val macros = project
    .aggregate(libs)
    .dependsOn(libs)

lazy val kickoff = project
    .aggregate(app)
    .dependsOn(app)
