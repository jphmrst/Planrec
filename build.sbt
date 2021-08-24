
name := "Planrec JM"
organization := "org.maraist"
version := "0.1.0"
scalaVersion := "3.0.2-RC1"

unmanagedSources / excludeFilter := ".#*"
Global / excludeLintKeys ++= Set(scalacOptions)

libraryDependencies ++= Seq(
  "org.scalactic" %% "scalactic" % "3.2.9",
  "org.scalatest" %% "scalatest" % "3.2.9" % "test",
  // "org.maraist" %% "scala-latex" % "1.1.1",
  "org.maraist" %% "misc-utils" % "1.0.1"
)

Compile / doc / scalacOptions ++= Seq(
  "-doc-root-content", "rootdoc.txt",
  "-groups"
)

val main = Project(id = "planrec", base = file("."))
  .dependsOn(RootProject(file("/home/jm/Lib/Scala/Automata")))
  .dependsOn(RootProject(file("/home/jm/Lib/Scala/LaTeX")))
