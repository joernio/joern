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
  "io.github.plume-oss"    % "plume" % "0.5.11" exclude("io.github.plume-oss", "cpgconv"),

  "com.lihaoyi" %% "requests" % "0.6.5",
  "com.github.scopt" %% "scopt" % "3.7.1",
  "com.github.pathikrit" %% "better-files" % "3.9.1",
  "io.circe" %% "circe-generic" % "0.12.2",
  "org.reflections" % "reflections" % "0.9.12",
  "org.apache.logging.log4j" % "log4j-slf4j-impl" % "2.13.3" % Runtime,
  "org.scalatest" %% "scalatest" % "3.1.1" % Test,
)


enablePlugins(JavaAppPackaging)
scriptClasspath := Seq("*") //wildcard import from staged `lib` dir, for simplicity and also to avoid `line too long` error on windows

topLevelDirectory := Some(packageName.value)

Compile/packageDoc/mappings := Seq()

lazy val downloadFuzzyPreprocessor = taskKey[Option[File]]("Download the FuzzyC2CPG preprocessor")
downloadFuzzyPreprocessor := {
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

Universal/mappings ++= downloadFuzzyPreprocessor.value.map { fuzzyppdir =>
  NativePackagerHelper.contentOf(fuzzyppdir).map {
    case (binary, name) => binary -> s"/bin/$name"
  }
}.getOrElse(Nil)


lazy val cpgVersionFile = taskKey[File]("persist cpg version in file (e.g. for schema-extender)")
cpgVersionFile := {
  val ret = target.value / "cpg-version"
  better.files.File(ret.getPath).writeText(Versions.cpgVersion)
  ret
}
Universal/mappings += cpgVersionFile.value -> "schema-extender/cpg-version"

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

import sbt.Path.directory
Universal/packageBin/mappings ++= directory(new File("joern-cli/src/main/resources/scripts"))
