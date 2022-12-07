import sbt._

object Dependencies {
  val cats = "org.typelevel" %% "cats-core" % "2.9.0"
  val catsParse = "org.typelevel" %% "cats-parse" % "0.3.8"
  val betterFiles = "com.github.pathikrit" % "better-files_2.13" % "3.9.1"
  val guava = "com.google.guava" % "guava" % "31.0.1"
  val mUnit = "org.scalameta" %% "munit" % "0.7.29"
  val monocleCore = "dev.optics" %% "monocle-core"  % "3.1.0"
  val monocleMacro = "dev.optics" %% "monocle-macro" % "3.1.0"
}
