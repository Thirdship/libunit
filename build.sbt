name := "libunit"

version := "1.0.5"

organization := "com.thirdship"

scalaVersion := "2.11.7"

// ScalaTest - Unit Testing with ScalaMock mocking
libraryDependencies ++= Seq(
	"org.scalamock" %% "scalamock-scalatest-support" % "3.2.2" % "test",
	"com.typesafe.scala-logging" %% "scala-logging" % "3.1.0",
	"org.slf4j" % "slf4j-simple" % "1.6.4",
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
