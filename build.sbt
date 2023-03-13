scalaVersion := "3.2.0"

name := "StarMap"

version := "1.0"

maintainer := "thomas.anberree@univ-rennes.fr"

run / fork := true

scalacOptions := Seq("-unchecked", "-deprecation")

libraryDependencies += "com.github.sbt" % "junit-interface" % "0.13.3"

libraryDependencies += "net.minidev" % "json-smart" % "2.4.8"
libraryDependencies += "org.mapsforge" % "mapsforge-map-awt" % "0.18.0"

/* To package project to target/universal-src/starmap-1.0.zip:
   sbt UniversalSrc/packageBin
 */
enablePlugins(JavaAppPackaging)

import NativePackagerHelper._

UniversalSrc / mappings += file("build.sbt") -> "build.sbt"
UniversalSrc / mappings ++= directory("lib")
UniversalSrc / mappings ++= directory("src")
UniversalSrc / mappings ++= directory("img")
UniversalSrc / mappings ++= directory("csv")
UniversalSrc / mappings += file(
  "project/plugins.sbt"
) -> "project/plugins.sbt"
UniversalSrc / mappings += file(
  "project/build.properties"
) -> "project/build.properties"
