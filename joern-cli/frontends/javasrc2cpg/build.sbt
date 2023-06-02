name := "javasrc2cpg"

scalaVersion       := "2.13.8"
crossScalaVersions := Seq("2.13.8", "3.3.0")

dependsOn(Projects.dataflowengineoss, Projects.x2cpg % "compile->compile;test->test")

libraryDependencies ++= Seq(
  "io.shiftleft" %% "codepropertygraph" % Versions.cpg,
  "io.joern" % "javaparser-symbol-solver-core" % "3.24.3-SL3", // custom build of our fork, sources at https://github.com/mpollmeier/javaparser
  "org.gradle"              % "gradle-tooling-api"         % Versions.gradleTooling,
  "org.scalatest"          %% "scalatest"                  % Versions.scalatest % Test,
  "org.projectlombok"       % "lombok"                     % "1.18.28",
  "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.4",
  "org.scala-lang.modules" %% "scala-parser-combinators"   % "2.2.0",
  "net.lingala.zip4j"       % "zip4j"                      % "2.11.5"
)

scalacOptions ++= Seq(
  "-deprecation" // Emit warning and location for usages of deprecated APIs.
)

enablePlugins(JavaAppPackaging, LauncherJarPlugin)
trapExit                      := false
Global / onChangedBuildSource := ReloadOnSourceChanges

lazy val packTestCode = taskKey[Unit]("Packs test code for JarTypeReader into jars.")
packTestCode := {
  import better.files._
  import net.lingala.zip4j.ZipFile
  import net.lingala.zip4j.model.ZipParameters
  import net.lingala.zip4j.model.enums.{CompressionLevel, CompressionMethod}
  import java.nio.file.Paths

  val pkgRoot              = "io"
  val testClassOutputPath  = Paths.get("joern-cli", "frontends", "javasrc2cpg", "target", "scala-2.13", "test-classes")
  val relativeTestCodePath = Paths.get(pkgRoot, "joern", "javasrc2cpg", "jartypereader", "testcode")

  File(testClassOutputPath.resolve(relativeTestCodePath)).list.filter(_.exists).foreach { testDir =>
    val tmpDir                     = File.newTemporaryDirectory()
    val tmpDirWithCorrectPkgStruct = File(tmpDir.path.resolve(relativeTestCodePath)).createDirectoryIfNotExists()
    testDir.copyToDirectory(tmpDirWithCorrectPkgStruct)
    val testRootPath = tmpDir.path.resolve(pkgRoot)

    val jarFilePath   = testClassOutputPath.resolve(testDir.name ++ ".jar")
    val jarFile       = new ZipFile(jarFilePath.toAbsolutePath.toString)
    val zipParameters = new ZipParameters()
    zipParameters.setCompressionMethod(CompressionMethod.DEFLATE)
    zipParameters.setCompressionLevel(CompressionLevel.NORMAL)
    zipParameters.setRootFolderNameInZip(relativeTestCodePath.toString)

    File(jarFilePath).delete(swallowIOExceptions = true)
    jarFile.addFolder(File(testRootPath).toJava)
  }
}
packTestCode := packTestCode.triggeredBy(Test / compile).value
