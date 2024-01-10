name := "javasrc2cpg"

dependsOn(Projects.dataflowengineoss % "compile->compile;test->test", Projects.x2cpg % "compile->compile;test->test")

libraryDependencies ++= Seq(
  "io.shiftleft"           %% "codepropertygraph"             % Versions.cpg,
  "com.github.javaparser"   % "javaparser-symbol-solver-core" % "3.25.6",
  "org.gradle"              % "gradle-tooling-api"            % Versions.gradleTooling,
  "org.scalatest"          %% "scalatest"                     % Versions.scalatest % Test,
  "org.projectlombok"       % "lombok"                        % "1.18.28",
  "org.scala-lang.modules" %% "scala-parallel-collections"    % "1.0.4",
  "org.scala-lang.modules" %% "scala-parser-combinators"      % "2.2.0",
  "net.lingala.zip4j"       % "zip4j"                         % "2.11.5"
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
  val testClassOutputPath  = target.value / ("scala-" + scalaVersion.value) / "test-classes"
  val relativeTestCodePath = Paths.get(pkgRoot, "joern", "javasrc2cpg", "jartypereader", "testcode")

  val jarFileRoot = target.value.toScala / "testjars"
  if (jarFileRoot.exists()) jarFileRoot.delete()
  jarFileRoot.createDirectories()

  File(testClassOutputPath.toPath.resolve(relativeTestCodePath)).list.filter(_.exists).foreach { testDir =>
    val tmpDir                     = File.newTemporaryDirectory()
    val tmpDirWithCorrectPkgStruct = File(tmpDir.path.resolve(relativeTestCodePath)).createDirectoryIfNotExists()
    testDir.copyToDirectory(tmpDirWithCorrectPkgStruct)
    val testRootPath = tmpDir.path.resolve(pkgRoot)

    val jarFilePath = jarFileRoot / (testDir.name + ".jar")
    if (jarFilePath.exists()) jarFilePath.delete()
    val jarFile       = new ZipFile(jarFilePath.canonicalPath)
    val zipParameters = new ZipParameters()
    zipParameters.setCompressionMethod(CompressionMethod.DEFLATE)
    zipParameters.setCompressionLevel(CompressionLevel.NORMAL)
    zipParameters.setRootFolderNameInZip(relativeTestCodePath.toString)
    jarFile.addFolder(File(testRootPath).toJava)
  }
}
packTestCode := packTestCode.triggeredBy(Test / compile).value
