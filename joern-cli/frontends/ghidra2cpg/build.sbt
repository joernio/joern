name := "ghidra2cpg"

dependsOn(Projects.dataflowengineoss % Test, Projects.x2cpg % "compile->compile;test->test", Projects.linterRules % ScalafixConfig)

libraryDependencies ++= Seq(
  "io.joern"          % "ghidra"                   % Versions.ghidra,
  "com.github.scopt" %% "scopt"                    % Versions.scopt,
  "commons-io"        % "commons-io"               % Versions.commonsIo,
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

// Because of the exclusions above, slf4j has no binding in this module (the fat jar only ships an slf4j 1.7
// binding, which slf4j 2.x ignores) and anything logged via slf4j is silently discarded. This module therefore
// logs via log4j directly (org.apache.logging.log4j), using the log4j version bundled in the ghidra fat jar;
// it is configured by log4j2-test.xml in tests and by conf/log4j2.xml in the CLI distribution. Side effect of
// the missing binding: slf4j prints a few 'No SLF4J providers were found' warnings when a test JVM starts.

enablePlugins(JavaAppPackaging, LauncherJarPlugin)

Test / scalacOptions += "-language:implicitConversions"
Test / testForkedParallel := false // ghidra is not thread-safe

// The serialFilterFactory is mandatory since ghidra 12.2: headless initialization installs it unconditionally and
// fails if the JVM has already pinned its builtin factory (which any forked sbt test JVM has). Side effect: once
// ghidra is initialized, the JVM-wide filter also rejects sbt's own end-of-run test protocol message, so every test
// run ends with a cosmetic "Internal error when running tests: ... Broken pipe" line. Test results are unaffected.
javaOptions := Seq(
  "-Djava.protocol.handler.pkgs=ghidra.framework.protocol",
  "-Djdk.serialFilterFactory=ghidra.framework.remote.GhidraSerialFilterFactory"
)
