import sbt.BareBuildSyntax.dependsOn

name := "pysrc2cpg"

dependsOn(
  Projects.dataflowengineoss  % "test->test",
  Projects.x2cpg              % "compile->compile;test->test",
  Projects.linterRules % ScalafixConfig
)

libraryDependencies ++= Seq(
  "io.shiftleft"           %% "codepropertygraph"          % Versions.cpg,
  "org.scala-lang.modules" %% "scala-parallel-collections" % Versions.scalaParallel,
  "org.scalatest"          %% "scalatest"                  % Versions.scalatest % Test
)

enablePlugins(JavaAppPackaging, LauncherJarPlugin)

val javaCCTask = taskKey[Seq[File]]("Generate compiler code with JavaCC")
javaCCTask := Def.uncached {
  import org.javacc.parser.{Main => JavaCCMain}
  val outputDir = (Compile / sourceManaged).value / "io" / "joern" / "pythonparser"
  val inputFile = baseDirectory.value / "pythonGrammar.jj"
  // sbt 2.x note: `fileInputs` no longer gates task re-runs (cached-task inputs hash as paths, not
  // content), so this task stays uncached and guards regeneration itself via a content stamp --
  // JavaCC only re-runs when pythonGrammar.jj actually changes.
  val stampFile = (Compile / sourceManaged).value / "pythonGrammar.jj.sha256"
  val stamp = java.util.HexFormat
    .of()
    .formatHex(java.security.MessageDigest.getInstance("SHA-256").digest(IO.readBytes(inputFile)))
  IO.createDirectory(outputDir)
  def generated = os.walk(os.Path(outputDir)).filter(p => os.isFile(p) && p.ext == "java").map(_.toIO)
  if (!(stampFile.exists() && IO.read(stampFile) == stamp) || generated.isEmpty) {
    JavaCCMain.mainProgram(Array(s"-OUTPUT_DIRECTORY=$outputDir", inputFile.toString))
    IO.write(stampFile, stamp)
  }
  generated
}

Compile / sourceGenerators += javaCCTask
