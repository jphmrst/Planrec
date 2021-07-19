
name := "Planrec JM"
organization := "org.maraist"
version := "0.1.0"
scalaVersion := "3.0.2-RC1"

unmanagedSources / excludeFilter := ".#*"
Global / excludeLintKeys ++= Set(scalacOptions)

libraryDependencies ++= Seq(
  "org.scalactic" %% "scalactic" % "3.2.9",
  "org.scalatest" %% "scalatest" % "3.2.9" % "test",
  "org.maraist" %% "scala-latex" % "1.1.1",
  "org.maraist" %% "misc-utils" % "1.0.1"
)

Compile / doc / scalacOptions ++= Seq(
  "-doc-root-content", "rootdoc.txt",
  "-groups"
)

lazy val automata = RootProject(file("/home/jm/Lib/Scala/Automata"))

val main = Project(id = "planrec", base = file("."))
  .dependsOn(automata)

// You can use Scaladex, an index of all known published Scala libraries. There,
// after you find the library you want, you can just copy/paste the dependency
// information that you need into your build file. For example, on the
// scala/scala-parser-combinators Scaladex page,
// https://index.scala-lang.org/scala/scala-parser-combinators, you can copy/paste
// the sbt dependency from the sbt box on the right-hand side of the screen.

// IMPORTANT NOTE: while build files look _kind of_ like regular Scala, it's
// important to note that syntax in *.sbt files doesn't always behave like
// regular Scala. For example, notice in this build file that it's not required
// to put our settings into an enclosing object or class. Always remember that
// sbt is a bit different, semantically, than vanilla Scala.

