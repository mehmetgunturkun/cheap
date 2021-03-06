import sbt._

name := """cheap"""

version := "1.0"

scalaVersion := "2.11.7"

libraryDependencies += "joda-time" % "joda-time" % "2.9.1"

libraryDependencies += "org.specs2" %% "specs2-core" % "3.8.7" % "test"

shellPrompt := getPrompt()

def getPrompt(): (State => String) = { state: State =>
  "[" + scala.Console.CYAN + "cheap" + scala.Console.RESET + "] $ "
}