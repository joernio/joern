name := "ghidra2cpg"

dependsOn(Projects.dataflowengineoss, Projects.x2cpg % "compile->compile;test->test")

libraryDependencies ++= Seq(
  "com.github.scopt" %% "scopt"                    % "4.0.1",
  "commons-io"        % "commons-io"               % "2.11.0",
  "io.shiftleft"      % "ghidra"                   % "10.1_PUBLIC_20211210a",
  "io.shiftleft"     %% "codepropertygraph"        % Versions.cpg,
  "io.shiftleft"     %% "codepropertygraph-protos" % Versions.cpg,
  "org.scalatest"    %% "scalatest"                % Versions.scalatest % Test
)

enablePlugins(JavaAppPackaging, LauncherJarPlugin)

fork        := true
javaOptions := Seq("-Djava.protocol.handler.pkgs=ghidra.framework.protocol")
