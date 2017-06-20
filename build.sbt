import sbt._
import sbt.Keys._

scalaVersion := "2.11.8"

organization := "in.extrasalt"

name := "scala-problems"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.3" % "test"
