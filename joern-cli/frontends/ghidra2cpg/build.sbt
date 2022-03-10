name := "ghidra2cpg"

dependsOn(Projects.dataflowengineoss, Projects.x2cpg % "compile->compile;test->test")

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

/* ghidra ships log4j-slf4j-impl in it's fat jar, so exclude it for the remainder 
 * of the project to avoid two slf4j bindings */
excludeDependencies += ExclusionRule(organization="org.apache.logging.log4j", name="log4j-slf4j-impl")

enablePlugins(JavaAppPackaging)

fork        := true
javaOptions := Seq("-Djava.protocol.handler.pkgs=ghidra.framework.protocol")
