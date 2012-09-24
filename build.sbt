////////////////////////////////////////////
//      ___    ____     __          __    //
//     /   |  / __ \   / /   ____ _/ /_   //
//    / /| | / /_/ /  / /   / __ `/ __ \  //
//   / ___ |/ _, _/  / /___/ /_/ / /_/ /  //
//  /_/  |_/_/ |_|  /_____/\__,_/_.___/   //
//                                        //
////////////////////////////////////////////

organization := "de.arlab"

name := "AR Lab"

version := "0.1"

libraryDependencies += "org.scalatest" %% "scalatest" % "1.8" % "test"

resolvers ++= Seq(
  "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/",
  "sbt-idea-repo" at "http://mpeltonen.github.com/maven/"
)

scalaVersion := "2.9.2"

initialCommands in console := """
   import de.arlab.formulas._
"""