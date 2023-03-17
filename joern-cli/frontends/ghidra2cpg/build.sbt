name := "ghidra2cpg"

dependsOn(Projects.dataflowengineoss, Projects.x2cpg % "compile->compile;test->test")

libraryDependencies ++= Seq(
  "com.github.scopt" %% "scopt"                    % "4.1.0",
  "commons-io"        % "commons-io"               % "2.11.0",
  "io.shiftleft"      % "ghidra"                   % "10.1_PUBLIC_20211210a",
  "io.shiftleft"     %% "codepropertygraph"        % Versions.cpg,
  "io.shiftleft"     %% "codepropertygraph-protos" % Versions.cpg,
  "org.scalatest"    %% "scalatest"                % Versions.scalatest % Test
)

// ghidra2cpg is a fat jar that already ships an old version of log4j, so we need
// to exclude the ones that we normally bring in... otherwise, tests are failing:
// java.lang.NoSuchMethodError: 'java.lang.ClassLoader[] org.apache.logging.log4j.util.LoaderUtil.getClassLoaders()'
excludeDependencies ++= Seq(
  ExclusionRule("org.apache.logging.log4j", "log4j-slf4j2-impl"),
  ExclusionRule("org.apache.logging.log4j", "log4j-core")
)

enablePlugins(JavaAppPackaging, LauncherJarPlugin)

fork        := true
javaOptions := Seq("-Djava.protocol.handler.pkgs=ghidra.framework.protocol")
