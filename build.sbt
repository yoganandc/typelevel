lazy val root = (project in file("."))
  .settings(
    scalaVersion := "2.13.10",
    libraryDependencies ++= Seq("org.scala-lang" % "scala-reflect" % scalaVersion.value)
  )
