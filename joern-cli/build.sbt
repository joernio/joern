enablePlugins(JavaAppPackaging)
enablePlugins(UniversalPlugin)

organization := "io.shiftleft"
name := "joern-cli"
maintainer := "fabs@shiftleft.io"

libraryDependencies ++= Seq(
  "io.shiftleft" %% "codepropertygraph" % Versions.cpgVersion,
  "io.shiftleft" %% "semanticcpg" % Versions.cpgVersion,
  "io.shiftleft" %% "console" % Versions.cpgVersion,
  "io.shiftleft" %% "dataflowengineoss" % Versions.cpgVersion,
  "io.shiftleft" %% "fuzzyc2cpg" % Versions.fuzzyc2cpgVersion,
  "com.lihaoyi" %% "ammonite" % "2.0.4" cross CrossVersion.full,
  "com.github.scopt" %% "scopt" % "3.7.1",
  "com.github.pathikrit" %% "better-files" % "3.8.0",
  "io.circe" %% "circe-generic" % "0.12.2",
  "org.reflections" % "reflections" % "0.9.12",
  "org.apache.logging.log4j" % "log4j-slf4j-impl" % "2.13.3" % Runtime,
  "org.scalatest" %% "scalatest" % "3.0.8" % Test,
)

excludeDependencies ++= Seq(
  // This project uses Logback in place of Log4j
  ExclusionRule("org.apache.logging.log4j", "log4j-slf4j-impl"),
  ExclusionRule("org.slf4j", "slf4j-simple"),
  ExclusionRule("ch.qos.logback", "logback-classic"),
)

enablePlugins(JavaAppPackaging)
scriptClasspath := Seq("*") //wildcard import from staged `lib` dir, for simplicity and also to avoid `line too long` error on windows

topLevelDirectory := Some(packageName.value)

mappings in (Compile, packageDoc) := Seq()

lazy val downloadFuzzyPreprocessor = taskKey[File]("Download the FuzzyC2CPG preprocessor")
downloadFuzzyPreprocessor := {
  val ppFilename = "fuzzyppcli.zip"
  val ppUrl = new URL(
    s"https://github.com/ShiftLeftSecurity/fuzzyc2cpg/releases/download/v${Versions.fuzzyc2cpgVersion}/$ppFilename")

  val ppOutputDir = file("fuzzyppcli")
  println(s"downloading $ppUrl")
  IO.unzipURL(ppUrl, ppOutputDir)

  ppOutputDir.listFiles().map(_.setExecutable(true))

  ppOutputDir
}

import NativePackagerHelper.contentOf
mappings in Universal ++= contentOf(downloadFuzzyPreprocessor.value).map {
  case (binary, name) => binary -> s"/bin/$name"
}

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

/* add schemas from codepropertygraph and schema-extender */
Universal/packageBin/mappings ++= {
  import better.files.File
  val joernCliStaged = (Universal/stage).value
  val schemaExtenderStaged = (Projects.schemaExtender/Universal/stage).value.toPath
  val tmpDir = File.newTemporaryDirectory("joern-cli-build").deleteOnExit

  val cpgJarName = s"io.shiftleft.codepropertygraph_${scalaBinaryVersion.value}-${Versions.cpgVersion}.jar"
  val cpgJar = joernCliStaged/"lib"/cpgJarName
  assert(cpgJar.exists, s"cpg jar not found at expected path: $cpgJar")
  IO.unzip(cpgJar, tmpDir.toJava, _.startsWith("schemas/"))

  for {
    dir <- List(tmpDir, File(schemaExtenderStaged))
    file <- dir.listRecursively
  } yield file.toJava -> s"schema-extender/${dir.relativize(file.path)}"
}
