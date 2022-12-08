import Dependencies._
val scala3Version = "3.2.1"
scalacOptions ++= (
  Seq(
    "-encoding", "UTF-8",
    "-source", "future"
  )
)

name := "Advent of Code 2022"

lazy val day01 = dayProject(1, "Calorie Counting")
lazy val day02 = dayProject(2, "Rock Paper Scissors")
lazy val day03 = dayProject(3, "Rucksack Reorganization")
lazy val day04 = dayProject(4, "Camp Cleanup")
lazy val day05 = dayProject(5, "Supply Stacks")
lazy val day06 = dayProject(6, "Tuning Trouble")
lazy val day07 = dayProject(7, "No Space Left On Device")
lazy val day08 = dayProject(8, "Treetop Tree House")


lazy val common = project
  .in(file("days/common"))
  .settings(
    name := f"Advent-of-Code 2022: Commons",
    version := "0.1.0",
    scalaVersion := scala3Version,
    libraryDependencies ++= Seq(
      betterFiles,
      catsParse,
      mUnit % "test"
    )
  )

def dayProject(day: Int, title: String = ""): Project = Project.apply(f"day_$day%02d", file(f"days/$day%02d"))
  .settings(
    name := f"AoC Day $day%2d" + (if (title.nonEmpty) s" - $title" else ""),
    version := "0.1.0",
    scalaVersion := scala3Version,
    libraryDependencies ++= Seq(
      cats,
      mUnit % "test"
    )
  )
  .dependsOn(common % "compile->compile;test->test")

def dayProject(day: Int, title: String, additionalDependencies: Seq[ModuleID]): Project  =
  dayProject(day, title).settings(
    libraryDependencies ++= additionalDependencies
  )
