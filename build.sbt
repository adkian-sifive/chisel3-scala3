// See LICENSE for license details.

import com.typesafe.tools.mima.core._

Compile / compile / logLevel := Level.Error

val scala3Version = "3.4.1"

lazy val commonSettings = Seq (
  organization := "edu.berkeley.cs",
  version := "3.5.6",
  autoAPIMappings := true,
  scalaVersion := scala3Version,
  crossScalaVersions := Seq("2.13.10", "2.12.17", scala3Version),
  scalacOptions := Seq("-rewrite", "-source:3.4-migration")
)

lazy val chiselSettings = Seq (
  name := "chisel3",
)

lazy val core = (project in file("core")).
  settings(commonSettings: _*).
  enablePlugins(BuildInfoPlugin).
  settings(
    buildInfoPackage := "chisel3",
    buildInfoUsePackageAsPath := true,
    buildInfoKeys := Seq[BuildInfoKey](buildInfoPackage, version, scalaVersion, sbtVersion)
  ).
  settings(
    libraryDependencies += "edu.berkeley.cs" % "firrtl_3" % "1.6-SNAPSHOT",
  ).
  settings(
    name := "chisel3-core",
    scalacOptions := scalacOptions.value ++ Seq(
      "-explaintypes",
      "-feature",
      "-language:reflectiveCalls",
      "-unchecked",
    )
  )

// This will always be the root project, even if we are a sub-project.
lazy val root = RootProject(file("."))

lazy val chisel = (project in file(".")).
  settings(commonSettings: _*).
  settings(chiselSettings: _*).
  dependsOn(core).
  aggregate(core).
  settings(
    scalacOptions in Test ++= Seq("-language:reflectiveCalls"),
  )

addCommandAlias("com", "all compile")
addCommandAlias("lint", "; compile:scalafix --check ; test:scalafix --check")
addCommandAlias("fix", "all compile:scalafix test:scalafix")
