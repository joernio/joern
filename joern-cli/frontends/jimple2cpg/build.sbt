name := "jimple2cpg"

dependsOn(Projects.dataflowengineoss % "compile->compile;test->test", Projects.x2cpg % "compile->compile;test->test")

// upgrade asm for jdk21 support
val asmVersion = "9.6"

libraryDependencies ++= Seq(
  "io.shiftleft"    %% "codepropertygraph" % Versions.cpg,
  "org.soot-oss"     % "soot"              % "4.4.1",
  "org.ow2.asm"      % "asm"               % asmVersion,
  "org.ow2.asm"      % "asm-commons"       % asmVersion,
  "org.ow2.asm"      % "asm-tree"          % asmVersion,
  "org.ow2.asm"      % "asm-util"          % asmVersion,
  "org.ow2.asm"      % "asm-analysis"      % asmVersion,
  "org.scalatest"   %% "scalatest"         % Versions.scalatest % Test,
  "org.benf"         % "cfr"               % "0.152",
)

enablePlugins(JavaAppPackaging, LauncherJarPlugin)
trapExit    := false
Test / fork := true
