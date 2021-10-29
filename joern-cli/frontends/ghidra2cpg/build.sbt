name := "ghidra2cpg"

enablePlugins(JavaAppPackaging)

libraryDependencies ++= Seq(
  "com.github.scopt" %% "scopt"                    % "3.7.1",
  "commons-io"        % "commons-io"               % "2.7",
  "io.shiftleft"      % "ghidra"                   % "10.0_PUBLIC_20210621",
  "io.shiftleft"     %% "codepropertygraph"        % Versions.cpg,
  "io.shiftleft"     %% "codepropertygraph-protos" % Versions.cpg,
  "io.shiftleft"     %% "dataflowengineoss"        % Versions.cpg,
  "io.shiftleft"     %% "semanticcpg"              % Versions.cpg,
  "org.scalatest" %% "scalatest" % Versions.scalatest % Test,
  "io.shiftleft" %% "semanticcpg-tests" % Versions.cpg % Test classifier "tests",
)

fork := true
javaOptions := Seq("-Djava.protocol.handler.pkgs=ghidra.framework.protocol")
