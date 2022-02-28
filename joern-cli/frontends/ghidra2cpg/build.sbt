name := "ghidra2cpg"

dependsOn(Projects.dataflowengineoss, Projects.x2cpg)

libraryDependencies ++= Seq(
  "com.github.scopt" %% "scopt"                    % "4.0.1",
  "commons-io"        % "commons-io"               % "2.11.0",
  "io.shiftleft"      % "ghidra"                   % "10.1_PUBLIC_20211210a",
  "io.shiftleft"     %% "codepropertygraph"        % Versions.cpg,
  "io.shiftleft"     %% "codepropertygraph-protos" % Versions.cpg,
  "io.shiftleft"     %% "semanticcpg"              % Versions.cpg,
  "org.scalatest"    %% "scalatest"                % Versions.scalatest % Test,
  "io.shiftleft"     %% "semanticcpg"              % Versions.cpg       % Test classifier "tests"
)

enablePlugins(JavaAppPackaging)

fork        := true
javaOptions := Seq("-Djava.protocol.handler.pkgs=ghidra.framework.protocol")
