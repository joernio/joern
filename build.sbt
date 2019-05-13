name := "joern"
organization := "io.shiftleft"
scalaVersion := "2.12.8"

val cpgVersion = "0.9.176"
val fuzzycVersion = "0.1.50"

libraryDependencies ++= Seq(
  "io.shiftleft" % "codepropertygraph" % cpgVersion,
  "io.shiftleft" % "query-primitives" % cpgVersion,
  "io.shiftleft" % "enhancements" % cpgVersion,
  "io.shiftleft" % "semanticcpg" % cpgVersion,
  "io.shiftleft" % "dataflowengine" % cpgVersion,
  "io.shiftleft" %% "fuzzyc2cpg" % fuzzycVersion,

  "com.github.scopt"   %% "scopt"          % "3.7.0",
  "com.github.pathikrit" %% "better-files"  % "3.1.0",

  "io.shiftleft" % "cpgqueryingtests" % cpgVersion % Test,
  "org.scalatest" %% "scalatest" % "3.0.3" % Test
)

resolvers += Resolver.mavenLocal
resolvers += "Sonatype OSS" at "https://oss.sonatype.org/content/repositories/public"

enablePlugins(JavaAppPackaging)
enablePlugins(HugoPlugin)

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
  val fiOpts = (Compile/doc/fileInputOptions).value

   val sOpts = Seq(
    "-language:implicitConversions",
    "-doc-root-content", "api-doc-root.txt",
    "-implicits")

  val xapis = apiMappings.value
  val options = sOpts ++ Opts.doc.externalAPI(xapis)
  val cp = data((Compile/dependencyClasspath).value).toList

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
    ZipUtil.unpack(
      SbtHelper.findJar(projectName, updateReport, SbtHelper.JarClassifier.Sources),
      inputFiles.toJava)
  }


  // slightly adapted from sbt's Default.scala `docTaskSettings`
  val srcs: Seq[JFile] =
    inputFiles.listRecursively
      .filter { file =>
        file.extension == Some(".java") || file.extension == Some(".scala")
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

scmInfo := Some(ScmInfo(url("https://github.com/ShiftLeftSecurity/joern"),
                     "scm:git@github.com:ShiftLeftSecurity/joern.git"
                     ))
homepage := Some(url("https://joern.io/"))
licenses := List("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0"))

sourceDirectory in Hugo := new java.io.File("docs")
baseURL in Hugo := uri("https://joern.io/docs")