enablePlugins(JavaAppPackaging)
enablePlugins(UniversalPlugin)

organization := "io.shiftleft"
name := "joern-cli"
maintainer := "fabs@shiftleft.io"

libraryDependencies ++= Seq(
  "io.shiftleft" %% "codepropertygraph" % Versions.cpgVersion,
  "io.shiftleft" %% "semanticcpg" % Versions.cpgVersion,
  "io.shiftleft" %% "console" % Versions.cpgVersion,
  "io.shiftleft" %% "console" % Versions.cpgVersion % Test classifier "tests",
  "io.shiftleft" %% "dataflowengineoss" % Versions.cpgVersion,
  "io.shiftleft" %% "fuzzyc2cpg" % Versions.cpgVersion,
  "com.lihaoyi" %% "ammonite" % "2.0.4" cross CrossVersion.full,
  "com.github.scopt" %% "scopt" % "3.7.1",
  "com.github.pathikrit" %% "better-files" % "3.8.0",
  "io.circe" %% "circe-generic" % "0.12.2",
  "org.reflections" % "reflections" % "0.9.12",
  "org.apache.logging.log4j" % "log4j-slf4j-impl" % "2.13.3" % Runtime,
  "org.scalatest" %% "scalatest" % "3.1.1" % Test,
)

enablePlugins(JavaAppPackaging)
scriptClasspath := Seq("*") //wildcard import from staged `lib` dir, for simplicity and also to avoid `line too long` error on windows

topLevelDirectory := Some(packageName.value)

mappings in (Compile, packageDoc) := Seq()

lazy val downloadFuzzyPreprocessor = taskKey[Option[File]]("Download the FuzzyC2CPG preprocessor")
downloadFuzzyPreprocessor := {
  import scala.util.{Try, Success, Failure}
  val log = streams.value.log
  val ppFilename = "fuzzyppcli.zip"
  val ppUrl = new URL(
    s"https://github.com/ShiftLeftSecurity/codepropertygraph/releases/download/v${Versions.cpgVersion}/$ppFilename")
  val ppOutputDir = file("fuzzyppcli")

  log.info(s"trying to download fuzzypp from $ppUrl")
  try {
    IO.unzipURL(ppUrl, ppOutputDir)
    ppOutputDir.listFiles().map(_.setExecutable(true))
    Some(ppOutputDir)
  } catch {
    case ex: Exception =>
      log.warn(s"unable to download fuzzypp from $ppUrl - if you are using a local release you can ignore this. Otherwise please investigate, since the cpg release build may have a problem.")
      log.trace(ex)
      None
  }
}

mappings in Universal ++= downloadFuzzyPreprocessor.value.map { fuzzyppdir =>
  NativePackagerHelper.contentOf(fuzzyppdir).map {
    case (binary, name) => binary -> s"/bin/$name"
  }
}.getOrElse(Nil)

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

  val cpgJarName = s"io.shiftleft.codepropertygraph-schema_${scalaBinaryVersion.value}-${Versions.cpgVersion}.jar"
  val cpgJar = joernCliStaged/"lib"/cpgJarName
  assert(cpgJar.exists, s"cpg jar not found at expected path: $cpgJar")
  IO.unzip(cpgJar, tmpDir.toJava, _.startsWith("schemas/"))

  for {
    dir <- List(tmpDir, File(schemaExtenderStaged))
    file <- dir.listRecursively
  } yield file.toJava -> s"schema-extender/${dir.relativize(file.path)}"
}

import sbt.Path.directory
Universal/packageBin/mappings ++= directory(new File("joern-cli/src/main/resources/scripts"))

lazy val foo = taskKey[Unit]("foo")
foo := {
  import better.files.File
  val joernCliStaged = (Universal/stage).value
  val schemaExtenderStaged = (Projects.schemaExtender/Universal/stage).value.toPath
  val tmpDir = File.newTemporaryDirectory("joern-cli-build").deleteOnExit

  val cpgJarName = s"io.shiftleft.codepropertygraph-schema_${scalaBinaryVersion.value}-${Versions.cpgVersion}.jar"
  val cpgJar = joernCliStaged/"lib"/cpgJarName
  assert(cpgJar.exists, s"cpg jar not found at expected path: $cpgJar")
  IO.unzip(cpgJar, tmpDir.toJava, _.startsWith("schemas/"))

  val res = for {
    dir <- List(tmpDir, File(schemaExtenderStaged))
    file <- dir.listRecursively
  } yield file.toJava -> s"schema-extender/${dir.relativize(file.path)}"
  res.foreach(println)
}
