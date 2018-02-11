name := "libunit"

version := "1.0.7"

organization := "com.thirdship"

scalaVersion := "2.11.8"

// ScalaTest - Unit Testing with ScalaMock mocking
libraryDependencies ++= Seq(
	"org.scalamock" %% "scalamock-scalatest-support" % "3.5.0" % "test",
	"com.typesafe.scala-logging" %% "scala-logging" % "3.7.2",
	"org.reflections" % "reflections" % "0.9.10",
	"org.atteo" % "evo-inflector" % "1.0.1",
	"com.thirdship" %% "name-generator" % "0.1.0"
)

publishTo := {
	val nexus = "https://sbt.johnstarich.com/"
	if (version.value.trim.endsWith("SNAPSHOT"))
		Some("snapshots" at nexus + "repository/maven-snapshots")
	else
		Some("releases" at nexus + "repository/maven-releases")
}

credentials += Credentials(Path.userHome / ".sbt" / ".credentials" / "thirdship")
