import sbt.BareBuildSyntax.dependsOn

name := "javasrc2cpg"

dependsOn(
  Projects.dataflowengineoss  % "test->test",
  Projects.x2cpg              % "compile->compile;test->test",
  Projects.linterRules % ScalafixConfig
)

libraryDependencies ++= Seq(
  "io.shiftleft"           %% "codepropertygraph"             % Versions.cpg,
  "com.github.javaparser"   % "javaparser-symbol-solver-core" % Versions.javaParser,
  "org.gradle"              % "gradle-tooling-api"            % Versions.gradleTooling,
  "org.scalatest"          %% "scalatest"                     % Versions.scalatest % Test,
  "org.projectlombok"       % "lombok"                        % Versions.lombok,
  "org.scala-lang.modules" %% "scala-parallel-collections"    % Versions.scalaParallel,
  "org.scala-lang.modules" %% "scala-parser-combinators"      % Versions.scalaParserCombinators,
  "net.lingala.zip4j"       % "zip4j"                         % Versions.zip4j,
  "org.ow2.asm"             % "asm"                           % Versions.asm
)

enablePlugins(JavaAppPackaging, LauncherJarPlugin)

lazy val packTestCode = taskKey[Unit]("Packs test code for JarTypeReader into jars.")
packTestCode := Def.uncached {
  val _ = (Test / compile).value // ensure the test classes are compiled before packaging them
  import better.files._
  import net.lingala.zip4j.ZipFile
  import net.lingala.zip4j.model.ZipParameters
  import net.lingala.zip4j.model.enums.{CompressionLevel, CompressionMethod}
  import java.nio.file.Paths

  val pkgRoot              = "io"
  val testClassOutputPath  = (Test / classDirectory).value
  val relativeTestCodePath = Paths.get(pkgRoot, "joern", "javasrc2cpg", "jartypereader", "testcode")

  val jarFileRoot = (baseDirectory.value / "target" / "testjars").toScala
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
// triggeredBy does not fire when `Test / compile` is served from the sbt 2.x task cache,
// so make test execution depend on the test jars explicitly instead (testLoader is the
// common choke point of test, testOnly and testQuick in sbt 2.x).
Test / testLoader := Def.uncached((Test / testLoader).dependsOn(packTestCode).value)
