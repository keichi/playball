name := "playboy-macros"

version := "1.0-SNAPSHOT"

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

resolvers += Resolver.sonatypeRepo("releases")

scalacOptions ++= Seq("-feature", "-deprecation")

libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-reflect" % "2.10.3",
  "org.scalamacros" %% "quasiquotes" % "2.0.1",
  "com.typesafe.play" %% "play" % "2.2.3",
  "com.typesafe.slick" %% "slick" % "2.0.3"
)

addCompilerPlugin("org.scalamacros" % "paradise" % "2.0.1" cross CrossVersion.full)
