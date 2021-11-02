name := "joern-cli"

dependsOn(
  Projects.console,
  Projects.console % "test->test",
)

libraryDependencies ++= Seq(
  "io.shiftleft" %% "codepropertygraph" % Versions.cpg,
  "io.shiftleft" %% "semanticcpg" % Versions.cpg,
  "io.shiftleft" %% "dataflowengineoss" % Versions.cpg,
  "io.shiftleft" %% "fuzzyc2cpg" % Versions.cpg, // only needed for joern-parse - TODO MP consider to remove?

  "com.lihaoyi" %% "requests" % "0.6.5",
  "com.github.scopt" %% "scopt" % "3.7.1",
  "com.github.pathikrit" %% "better-files" % "3.9.1",
  "io.circe" %% "circe-generic" % "0.12.2",
  "org.reflections" % "reflections" % "0.9.12",
  "org.apache.logging.log4j" % "log4j-slf4j-impl" % "2.13.3" % Runtime,
  "org.scalatest" %% "scalatest" % Versions.scalatest % Test,
)

enablePlugins(UniversalPlugin)
enablePlugins(JavaAppPackaging)
scriptClasspath := Seq("*") //wildcard import from staged `lib` dir, for simplicity and also to avoid `line too long` error on windows

topLevelDirectory := Some(packageName.value)

Compile/packageDoc/mappings := Seq()

lazy val javasrc2cpg = project.in(file("frontends/javasrc2cpg"))
Universal/mappings ++= NativePackagerHelper.contentOf((javasrc2cpg/stage).value).map {
  case (file, name) => file -> s"frontends/javasrc2cpg/$name"
}

lazy val c2cpg = project.in(file("frontends/c2cpg"))
Universal/mappings ++= NativePackagerHelper.contentOf((c2cpg/stage).value).map {
  case (file, name) => file -> s"frontends/c2cpg/$name"
}

lazy val fuzzyc2cpg = project.in(file("frontends/fuzzyc2cpg")).enablePlugins(JavaAppPackaging).settings(
  libraryDependencies += "io.shiftleft" %% "fuzzyc2cpg" % Versions.cpg,
  Compile/mainClass := Some("io.shiftleft.fuzzyc2cpg.FuzzyC2Cpg"),
)
Universal/mappings ++= NativePackagerHelper.contentOf((fuzzyc2cpg/stage).value).map {
  case (file, name) => file -> s"frontends/fuzzyc2cpg/$name"
}

Universal/mappings ++= NativePackagerHelper.contentOf((Projects.ghidra2cpg/stage).value).map {
  case (file, name) => (file, s"frontends/ghidra2cpg/$name")
}

lazy val js2cpg = project.in(file("frontends/js2cpg")).enablePlugins(JavaAppPackaging).settings(
  libraryDependencies += "io.shiftleft" %% "js2cpg" % Versions.js2cpg,
  Compile/mainClass := Some("io.shiftleft.js2cpg.core.Js2CpgMain"),
)
Universal/mappings ++= NativePackagerHelper.contentOf((js2cpg/stage).value).map {
  case (file, name) => file -> s"frontends/js2cpg/$name"
}

lazy val jimple2cpg = project.in(file("frontends/jimple2cpg")).enablePlugins(JavaAppPackaging).settings(
  libraryDependencies ++= Seq(
    "io.joern" %% "jimple2cpg" % Versions.jimple2cpg,
    "org.apache.logging.log4j" % "log4j-slf4j-impl" % "2.13.3",
  ),
  Compile/mainClass := Some("io.joern.jimple2cpg.Main"),
)
Universal/mappings ++= NativePackagerHelper.contentOf((jimple2cpg/stage).value).map {
  case (file, name) => (file, s"frontends/jimple2cpg/$name")
}

lazy val downloadFuzzyPreprocessor = taskKey[Option[File]]("Download the FuzzyC2CPG preprocessor")
downloadFuzzyPreprocessor := {
  val log = streams.value.log
  val ppFilename = "fuzzyppcli.zip"
  val ppUrl = new URL(
    s"https://github.com/ShiftLeftSecurity/codepropertygraph/releases/download/v${Versions.cpg}/$ppFilename")
  val ppOutputDir = file("fuzzyppcli")

  log.info(s"trying to download fuzzypp from $ppUrl")
  try {
    IO.unzipURL(ppUrl, ppOutputDir)
    ppOutputDir.listFiles().map(_.setExecutable(true))
    Some(ppOutputDir)
  } catch {
    case ex: Exception =>
      log.warn(s"unable to download fuzzypp from $ppUrl - if you are using a local release you can ignore this, but note that you won't be able to use fuzzypp.")
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
  better.files.File(ret.getPath)
    .createIfNotExists(createParents = true)
    .writeText(Versions.cpg)
  ret
}
Universal/mappings += cpgVersionFile.value -> "schema-extender/cpg-version"

lazy val generateScaladocs = taskKey[File]("generate scaladocs from combined project sources")
generateScaladocs := {
  import better.files._
  import java.io.{File => JFile, PrintWriter}
  import sbt.internal.inc.AnalyzingCompiler
  import sbt.internal.util.Attributed.data
  import net.lingala.zip4j.ZipFile
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
    "semanticcpg"
  ).foreach { projectName =>
    val jar = SbtHelper.findJar(s"${projectName}_2.13", updateReport, SbtHelper.JarClassifier.Sources)
    new ZipFile(jar).extractAll(inputFiles.pathAsString)
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

Universal/packageBin/mappings ++= sbt.Path.directory(new File("joern-cli/src/main/resources/scripts"))

maintainer := "fabs@shiftleft.io"
