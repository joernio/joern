name := "pysrc2cpg"

scalaVersion := "2.13.8"

dependsOn(Projects.x2cpg % "compile->compile;test->test")

libraryDependencies ++= Seq(
  "io.shiftleft"            %% "codepropertygraph"          % Versions.cpg,
  "org.rogach"              %% "scallop"                    % "4.1.0",
  "org.scala-lang.modules"  %% "scala-parallel-collections" % "1.0.4",
  "org.apache.logging.log4j" % "log4j-slf4j-impl"           % Versions.log4j     % Runtime,
  "org.scalatest"           %% "scalatest"                  % Versions.scalatest % Test
)

enablePlugins(JavaAppPackaging, LauncherJarPlugin)
trapExit                      := false
Global / onChangedBuildSource := ReloadOnSourceChanges

val javaCCTask = taskKey[Seq[File]]("Generate compiler code with JavaCC")
javaCCTask / fileInputs += baseDirectory.value.toGlob / "pythonGrammar.jj"
javaCCTask := {
  import org.javacc.parser.{Main => JavaCCMain}
  import better.files._
  val outputDir       = (Compile / sourceManaged).value / "io" / "joern" / "pythonparser"
  val inputFileOption = (javaCCTask.inputFileChanges.created ++ javaCCTask.inputFileChanges.modified).headOption
  if (inputFileOption.isDefined) {
    JavaCCMain.mainProgram(Array(s"-OUTPUT_DIRECTORY=$outputDir", inputFileOption.get.toString))
  }
  os.walk(os.Path(outputDir)).filter(path => os.isFile(path) && path.ext == "java").map(_.toIO)
}

Compile / sourceGenerators += javaCCTask
