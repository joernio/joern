name := "pysrc2cpg"

scalaVersion       := "2.13.8"
crossScalaVersions := Seq("2.13.8", "3.3.0")

dependsOn(Projects.dataflowengineoss, Projects.x2cpg % "compile->compile;test->test")

libraryDependencies ++= Seq(
  "io.shiftleft"           %% "codepropertygraph"          % Versions.cpg,
  "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.4",
  "org.scalatest"          %% "scalatest"                  % Versions.scalatest % Test
)

enablePlugins(JavaAppPackaging, LauncherJarPlugin)
trapExit                      := false
Global / onChangedBuildSource := ReloadOnSourceChanges

val javaCCTask = taskKey[Seq[File]]("Generate compiler code with JavaCC")
javaCCTask / fileInputs += baseDirectory.value.toGlob / "pythonGrammar.jj"
javaCCTask := {
  import org.javacc.parser.{Main => JavaCCMain}
  val outputDir       = (Compile / sourceManaged).value / "io" / "joern" / "pythonparser"
  val inputFileOption = javaCCTask.inputFiles.head
  if (
    !outputDir.exists() ||
    javaCCTask.inputFileChanges.created.nonEmpty ||
    javaCCTask.inputFileChanges.modified.nonEmpty
  ) JavaCCMain.mainProgram(Array(s"-OUTPUT_DIRECTORY=$outputDir", inputFileOption.toString))
  os.walk(os.Path(outputDir)).filter(path => os.isFile(path) && path.ext == "java").map(_.toIO)
}

Compile / sourceGenerators += javaCCTask
