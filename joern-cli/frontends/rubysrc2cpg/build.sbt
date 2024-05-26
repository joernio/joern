import com.typesafe.config.{Config, ConfigFactory}

name := "rubysrc2cpg"

dependsOn(Projects.dataflowengineoss % "compile->compile;test->test", Projects.x2cpg % "compile->compile;test->test")

lazy val appProperties = settingKey[Config]("App Properties")
appProperties := {
  val path            = (Compile / resourceDirectory).value / "application.conf"
  val applicationConf = ConfigFactory.parseFile(path).resolve()
  applicationConf
}

lazy val joernTypeStubsVersion = settingKey[String]("joern_type_stub version")
joernTypeStubsVersion := appProperties.value.getString("rubysrc2cpg.joern_type_stubs_version")

libraryDependencies ++= Seq(
  "io.shiftleft" %% "codepropertygraph" % Versions.cpg,
  "org.apache.commons" % "commons-compress" % Versions.commonsCompress, // For unpacking Gems with `--download-dependencies`
  "org.scalatest" %% "scalatest"      % Versions.scalatest % Test,
  "org.antlr"      % "antlr4-runtime" % Versions.antlr
)

enablePlugins(JavaAppPackaging, LauncherJarPlugin, Antlr4Plugin)

Antlr4 / antlr4Version    := Versions.antlr
Antlr4 / antlr4GenVisitor := true
Antlr4 / javaSource       := (Compile / sourceManaged).value

lazy val joernTypeStubsDlUrl = settingKey[String]("joern_type_stubs download url")
joernTypeStubsDlUrl := s"https://github.com/joernio/joern-type-stubs/releases/download/v${joernTypeStubsVersion.value}/"

lazy val joernTypeStubsDlTask = taskKey[Unit]("Download joern-type-stubs")
joernTypeStubsDlTask := {
  val joernTypeStubsDir = baseDirectory.value / "type_stubs"
  val fileName          = "rubysrc_builtin_types.zip"
  val shaFileName       = s"$fileName.sha512"

  joernTypeStubsDir.mkdir()

  DownloadHelper.ensureIsAvailable(s"${joernTypeStubsDlUrl.value}$fileName", joernTypeStubsDir / fileName)
  DownloadHelper.ensureIsAvailable(s"${joernTypeStubsDlUrl.value}$shaFileName", joernTypeStubsDir / shaFileName)

  val typeStubsFile = better.files.File(joernTypeStubsDir.getAbsolutePath) / fileName
  val checksumFile  = better.files.File(joernTypeStubsDir.getAbsolutePath) / shaFileName

  val typestubsSha = typeStubsFile.sha512

  // Checksum file must contain exactly 1 line, if more or less we automatically fail.
  if (checksumFile.lineIterator.size != 1) {
    throw new IllegalStateException("Checksum File should only have 1 line")
  }

  // Checksum from terminal adds the filename to the line, so we split on whitespace to get the checksum
  // separate from the filename
  if (checksumFile.lineIterator.next().split("\\s+")(0).toUpperCase != typestubsSha) {
    throw new Exception("Checksums do not match for type stubs!")
  }

  val distDir = (Universal / stagingDirectory).value / "type_stubs"
  distDir.mkdirs()
  IO.copyDirectory(joernTypeStubsDir, distDir)
}

Compile / compile := ((Compile / compile) dependsOn joernTypeStubsDlTask).value

Universal / packageName       := name.value
Universal / topLevelDirectory := None

githubOwner      := "Privado-Inc"
githubRepository := "joern"
credentials +=
  Credentials(
    "GitHub Package Registry",
    "maven.pkg.github.com",
    "Privado-Inc",
    sys.env.getOrElse("GITHUB_TOKEN", "N/A")
  )
