name := "jimple2cpg"

dependsOn(Projects.dataflowengineoss % "compile->compile;test->test", Projects.x2cpg % "compile->compile;test->test")

val sootUpVersion = "1.1.2"

libraryDependencies ++= Seq(
  "io.shiftleft"  %% "codepropertygraph"    % Versions.cpg,
  "org.soot-oss"   % "sootup.core"          % sootUpVersion,
  "org.soot-oss"   % "sootup.java.core"     % sootUpVersion,
  "org.soot-oss"   % "sootup.java.bytecode" % sootUpVersion,
  "org.scalatest" %% "scalatest"            % Versions.scalatest % Test
)

enablePlugins(JavaAppPackaging, LauncherJarPlugin)
trapExit    := false
Test / fork := true

// The below is necessary for dex2jar, which is a transitive dependency of SootUp
resolvers += "SciJava Public Repository" at "https://maven.scijava.org/content/repositories/public/"
