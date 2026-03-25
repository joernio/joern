import com.typesafe.sbt.packager.Keys.stagingDirectory

name := "abap2cpg"

dependsOn(
  Projects.dataflowengineoss % "compile->compile;test->test",
  Projects.x2cpg             % "compile->compile;test->test",
  Projects.linterRules       % ScalafixConfig
)

libraryDependencies ++= Seq(
  "io.shiftleft"  %% "codepropertygraph" % Versions.cpg,
  "com.lihaoyi"   %% "ujson"             % "4.1.0",
  "org.scalatest" %% "scalatest"         % Versions.scalatest % Test
)

enablePlugins(JavaAppPackaging, LauncherJarPlugin)

// Binary names per platform (produced by `npm run build` in this directory)
lazy val AbapgenWinX86   = "abapgen-win.exe"
lazy val AbapgenLinuxX86 = "abapgen-linux"
lazy val AbapgenLinuxArm = "abapgen-linux-arm"
lazy val AbapgenMacX86   = "abapgen-macos"
lazy val AbapgenMacArm   = "abapgen-macos-arm"

lazy val abapgenBinaryNames = taskKey[Seq[String]]("abapgen binary names for current or all platforms")
abapgenBinaryNames := {
  if (sys.props.get("ALL_PLATFORMS").contains("TRUE")) {
    Seq(AbapgenWinX86, AbapgenLinuxX86, AbapgenLinuxArm, AbapgenMacX86, AbapgenMacArm)
  } else {
    (Environment.operatingSystem, Environment.architecture) match {
      case (Environment.OperatingSystemType.Windows, _)                                => Seq(AbapgenWinX86)
      case (Environment.OperatingSystemType.Linux, Environment.ArchitectureType.X86)   => Seq(AbapgenLinuxX86)
      case (Environment.OperatingSystemType.Linux, Environment.ArchitectureType.ARMv8) => Seq(AbapgenLinuxArm)
      case (Environment.OperatingSystemType.Mac, Environment.ArchitectureType.X86)     => Seq(AbapgenMacX86)
      case (Environment.OperatingSystemType.Mac, Environment.ArchitectureType.ARMv8)   => Seq(AbapgenMacArm)
      case _                                                                            => Seq(AbapgenLinuxX86)
    }
  }
}

// Check that abapgen binaries exist; instruct developer to build them if not.
// Build with: npm install && npm run build   (requires Node.js + npx)
lazy val abapgenCheckTask = taskKey[Unit]("Check abapgen binaries exist in bin/astgen/")
abapgenCheckTask := {
  val astGenDir = baseDirectory.value / "bin" / "astgen"
  val missing = abapgenBinaryNames.value.filterNot(name => (astGenDir / name).exists())
  if (missing.nonEmpty) {
    sys.error(
      s"""abapgen binary/binaries not found in ${astGenDir}: ${missing.mkString(", ")}
         |Build them first by running inside ${baseDirectory.value}:
         |  npm install
         |  npm run build
         |""".stripMargin
    )
  }
  // Copy into universal stage dir
  val distDir = (Universal / stagingDirectory).value / "bin" / "astgen"
  distDir.mkdirs()
  IO.copyDirectory(astGenDir, distDir, preserveExecutable = true)
}

Compile / compile := ((Compile / compile) dependsOn abapgenCheckTask).value

lazy val abapgenSetAllPlatforms = taskKey[Unit]("Set ALL_PLATFORMS flag")
abapgenSetAllPlatforms := { System.setProperty("ALL_PLATFORMS", "TRUE") }

stage := Def
  .sequential(abapgenSetAllPlatforms, Universal / stage)
  .andFinally(System.setProperty("ALL_PLATFORMS", "FALSE"))
  .value

Universal / packageName       := name.value
Universal / topLevelDirectory := None
