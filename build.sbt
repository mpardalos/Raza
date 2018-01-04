name := "Raza"

version := "0.1"

scalaVersion := "2.12.4"

scalacOptions ++= Seq("-unchecked", "-deprecation")

watchSources += baseDirectory.value / "examples.rz"
