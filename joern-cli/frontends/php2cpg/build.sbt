import sbt.util.CacheImplicits.given

import com.typesafe.sbt.packager.Keys.stagingDirectory

val upstreamParserBinName  = "php-parser.phar"
val versionedParserBinName = s"php-parser-${Versions.phpParser}.phar"

lazy val phpParserDlUrl = settingKey[String]("php-parser download url")

lazy val phpParserDownloadTask = taskKey[Seq[xsbti.HashedVirtualFileRef]]("Download the php-parser phar")

lazy val phpParserStageTask =
  taskKey[Unit]("Stage php-parser into bin/php-parser and the Universal staging directory")

lazy val php2cpg = (project in file("."))
  .dependsOn(
    Projects.dataflowengineoss % "test->test",
    Projects.x2cpg % "compile->compile;test->test",
    Projects.linterRules % ScalafixConfig
  )
  .enablePlugins(JavaAppPackaging, LauncherJarPlugin)
  .settings(
    name := "php2cpg",
    phpParserDlUrl := s"https://github.com/joernio/PHP-Parser/releases/download/v${Versions.phpParser}/",

    libraryDependencies ++= Seq(
      "com.lihaoyi"       %% "upickle"                % Versions.upickle,
      "com.lihaoyi"       %% "ujson"                  % Versions.upickle,
      "io.shiftleft"      %% "codepropertygraph"      % Versions.cpg,
      "com.github.sh4869" %% "semver-parser-scala"    % Versions.semverParser,
      "org.scalatest"     %% "scalatest"              % Versions.scalatest % Test,
      "com.github.albfernandez" % "juniversalchardet" % Versions.juniversalchardet
    ),
    phpParserDownloadTask := DownloadHelper
      .downloadArtifacts(
        target.value / "php-parser-download",
        phpParserDlUrl.value,
        Seq(upstreamParserBinName),
        fileConverter.value,
        executable = false
      )
      .map { vf => Def.declareOutput(vf); vf },
    phpParserStageTask := Def.uncached {
      val conv           = fileConverter.value
      val downloaded     = phpParserDownloadTask.value.map(ref => conv.toPath(ref).toFile)
      val wrapperContent = s"<?php\nrequire('$versionedParserBinName');?>"
      Seq(
        baseDirectory.value / "bin" / "php-parser",
        (Universal / stagingDirectory).value / "bin" / "php-parser"
      ).foreach { dir =>
        dir.mkdirs()
        downloaded.foreach(src => DownloadHelper.copyIfChanged(src, dir / versionedParserBinName, executable = false))
        val wrapper = dir / "php-parser.php"
        if (!wrapper.isFile || IO.read(wrapper) != wrapperContent) {
          IO.write(wrapper, wrapperContent)
        }
      }
    },

    Compile / compile := Def.uncached { ((Compile / compile).dependsOn(phpParserStageTask)).value },

    /** write the php parser version to the manifest for downstream usage */
    Compile / packageBin / packageOptions +=
      Package.ManifestAttributes("PHP-Parser-Version" -> Versions.phpParser)
  )
