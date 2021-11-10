
name := "Planrec JM"
organization := "org.maraist"
version := "0.1.0"
scalaVersion := "3.1.0"

unmanagedSources / excludeFilter := ".#*"
Global / excludeLintKeys ++= Set(scalacOptions)

libraryDependencies ++= Seq(
  "org.typelevel" %% "paiges-core" % "0.4.2", // Remove when using automata from Scaladex again
  "org.scalactic" %% "scalactic" % "3.2.9",
  "org.scalatest" %% "scalatest" % "3.2.9" % "test",
  "org.maraist" %% "scala-latex" % "2.0.0",
  "org.maraist" %% "scala-automata" % "0.3.0",
  "org.maraist" %% "misc-utils" % "1.0.1"
)

Compile / doc / scalacOptions ++= Seq(
  "-doc-root-content", "rootdoc.txt",
  "-groups"
)

val main = Project(id = "planrec", base = file("."))
//  .dependsOn(RootProject(file("/home/jm/Lib/Scala/Automata/")))
