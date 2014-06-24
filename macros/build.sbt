name := "playboy-macros"

version := "1.0-SNAPSHOT"

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

resolvers += Resolver.sonatypeRepo("releases")

libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-reflect" % "2.10.3",
  "org.scalamacros" %% "quasiquotes" % "2.0.0",
  "com.typesafe.play" %% "play" % "2.2.3"
)

addCompilerPlugin("org.scalamacros" % "paradise" % "2.0.0" cross CrossVersion.full)
