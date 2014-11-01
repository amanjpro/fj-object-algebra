
name := "FJAlgebra"


version := "1.0"

scalaVersion := "2.11.1"

scalacOptions ++= Seq("-unchecked", "-feature", "-deprecation") 

libraryDependencies += "org.scalatest" % "scalatest_2.10" % "2.0" % "test"

libraryDependencies += 
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.2"

mainClass in (Compile, packageBin) := Some("ch.usi.inf.l3.fj.parser.FJCompiler")

