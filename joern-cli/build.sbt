enablePlugins(JavaAppPackaging)
enablePlugins(UniversalPlugin)

organization := "io.shiftleft"
name := "joern-cli"
maintainer := "fabs@shiftleft.io"

libraryDependencies ++= Seq(
  "io.shiftleft" %% "codepropertygraph" % Versions.cpgVersion,
  "io.shiftleft" %% "semanticcpg" % Versions.cpgVersion,
  "io.shiftleft" %% "console" % Versions.cpgVersion,
  "io.shiftleft" %% "dataflowengine" % Versions.cpgVersion,
  "io.shiftleft" %% "fuzzyc2cpg" % Versions.fuzzyc2cpgVersion,
  "com.github.scopt" %% "scopt" % "3.7.1",
  "com.github.pathikrit" %% "better-files" % "3.1.0",
  "org.scalatest" %% "scalatest" % "3.0.8" % Test
)

// Weird hack to fix https://github.com/scala/bug/issues/10058
// SBT messes up 'java.class.path' during tests.
// Hence, 'new ScriptEngineManager().getEngineByName("scala")' always returns 'null'.
// With this fix, scala-compiler-VERSION.jar is back on track and the engine is found.
lazy val fixJavaClasspath = taskKey[String]("fix the Java classpath during tests")
fixJavaClasspath := {
  val cp = (fullClasspath in Test).value.map(x => x.data.getAbsolutePath).mkString(":")
  System.setProperty("java.class.path", cp)
}

test in Test := (test in Test).dependsOn(fixJavaClasspath).value

enablePlugins(JavaAppPackaging)

topLevelDirectory := Some(packageName.value)

mappings in (Compile, packageDoc) := Seq()

lazy val generateScaladocs = taskKey[File]("generate scaladocs from combined project sources")
generateScaladocs := {

  import better.files._
  import java.io.{File => JFile, PrintWriter}
  import org.zeroturnaround.zip.ZipUtil
  import sbt.internal.inc.AnalyzingCompiler
  import sbt.internal.util.Attributed.data
  import sbt.internal.CommandStrings.ExportStream

  val updateReport = updateClassifiers.value
  val label = "Joern API documentation"
  val s = streams.value
  val out = target.value / "api"
  val fiOpts = (Compile / doc / fileInputOptions).value

  val sOpts = Seq("-language:implicitConversions", "-doc-root-content", "api-doc-root.txt", "-implicits")

  val xapis = apiMappings.value
  val options = sOpts ++ Opts.doc.externalAPI(xapis)
  val cp = data((Compile / dependencyClasspath).value).toList

  val inputFilesRelativeDir = target.value + "/inputFiles"
  val inputFiles = File(inputFilesRelativeDir)
  if (inputFiles.exists) inputFiles.delete()
  inputFiles.createDirectory()

  /* extract sources-jar dependencies */
  List(
    "codepropertygraph",
    "query-primitives",
    "enhancements",
    "semanticcpg"
  ).foreach { projectName =>
    ZipUtil.unpack(SbtHelper.findJar(projectName, updateReport, SbtHelper.JarClassifier.Sources), inputFiles.toJava)
  }

  // slightly adapted from sbt's Default.scala `docTaskSettings`
  val srcs: Seq[JFile] =
    inputFiles.listRecursively
      .filter { file =>
        file.extension.contains(".java") || file.extension.contains(".scala")
      }
      .map(_.toJava)
      .toSeq

  def exportedPW(w: PrintWriter, command: String): Seq[String] => Unit =
    args => w.println((command +: args).mkString(" "))

  def exportedTS(s: TaskStreams, command: String): Seq[String] => Unit = args => {
    val w = s.text(ExportStream)
    try exportedPW(w, command)
    finally w.close()
  }

  val runDoc = Doc.scaladoc(label, s.cacheStoreFactory.sub("scala"), compilers.value.scalac match {
    case ac: AnalyzingCompiler => ac.onArgs(exportedTS(s, "scaladoc"))
  }, fiOpts)

  runDoc(srcs, cp, out, options, maxErrors.value, s.log)

  out
}
