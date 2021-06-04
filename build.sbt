
scalaVersion := "3.0.0"

name := "Planrec JM"
organization := "org.maraist"
version := "1.0"

Compile / doc / scalacOptions ++= Seq(
  "-doc-root-content", "src/main/rootdoc.txt",
  "-external-mappings:" ++ (
    ".*scala.*::scaladoc3::" ++ "http://dotty.epfl.ch/api/"

    // ++ "/home/jm/Lib/Scala/scalatest-app_2.11-3.0.1.jar#"
    //   ++ "http://doc.scalatest.org/3.0.0/,"
  ),
  "-skip-by-id:Scratch,<empty>"
)

unmanagedSources / excludeFilter := ".#*",

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

// ============================================================================


// lazy val root = (project in file(".")).
//   settings(
//     inThisBuild(List(
//       organization := "ch.epfl.scala",
//       scalaVersion := "2.13.3"
//     )),
//     name := "hello-world"
//   )

