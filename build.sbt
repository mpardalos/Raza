name := "Raza"

version := "0.1"

scalaVersion := "2.12.4"

scalacOptions ++= Seq("-unchecked", "-deprecation")

watchSources += baseDirectory.value / "examples.rz"

// This might be very stupid
val SbtFile = ".*sbt.*".r

assemblyMergeStrategy in assembly := {
    case SbtFile() => MergeStrategy.discard
    case x =>
        val oldStrategy = (assemblyMergeStrategy in assembly).value
        oldStrategy(x)
}
