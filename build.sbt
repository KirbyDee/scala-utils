name := "ScalaUtils"

// kirbydee utils project
lazy val utils = (project in file("."))
        .settings(Commons.settings: _*)
        .settings(
            libraryDependencies ++= Dependencies.common
        )
        .aggregate(macros)
        .dependsOn(macros)

// macros
lazy val macros = project