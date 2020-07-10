lazy val commonSettings = Seq(
  name := "Anemone",
  organization := "org.coordinam",
  version := "1.0",
  scalaVersion := "2.12.8"
)

lazy val abData = (project in file("ab_data"))
  .settings(
    commonSettings,
    // other settings
  )

lazy val abParser = (project in file("ab_parser"))
  .settings(
    commonSettings,
    // other settings
  )
  .dependsOn(abData % "test->test;compile->compile" )


lazy val abBlackboard = (project in file("ab_blackboard"))
  .settings(
    commonSettings,
    // other settings
  )
  .dependsOn(abData % "test->test;compile->compile",
             abParser % "test->test;compile->compile" )


lazy val abScene = (project in file("ab_scene"))
  .settings(
    commonSettings,
    // other settings
  )
  .dependsOn(abData % "test->test;compile->compile" )


lazy val abSimulators = (project in file("ab_simulators"))
  .settings(
    commonSettings,
    // other settings
  )
  .dependsOn(abData % "test->test;compile->compile",
             abBlackboard % "test->test;compile->compile",
	     abScene % "test->test;compile->compile" )

lazy val abWindowAgents = (project in file("ab_window_agents"))
  .settings(
    commonSettings,
    // other settings
  )
  .dependsOn(abData % "test->test;compile->compile",
	     abParser % "test->test;compile->compile",
             abBlackboard % "test->test;compile->compile",
	     abScene % "test->test;compile->compile",
     	     abSimulators % "test->test;compile->compile" )

lazy val root = (project in file("."))
  .aggregate(abData,abParser,abBlackboard,abScene,abSimulators,abWindowAgents)

// comment to see the different classes

// set the main class for 'sbt run'
mainClass in (Compile, run) := Some("ab_window_agents.InteractiveBlackboard")

// set the main class for packaging the main jar
mainClass in (Compile, packageBin) := Some("ab_window_agents.InteractiveBlackboard")





