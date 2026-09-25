import sbt.BareBuildSyntax.dependsOn

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

// Since ghidra 12.2, headless initialization installs a JVM-wide serial filter factory unconditionally and fails
// if another factory has been pinned already (i.e. once any ObjectInputStream was created). Do NOT set
// -Djdk.serialFilterFactory: the JDK instantiates that class eagerly through the system class loader, and with
// sbt 2's JSON-based test worker nothing pins the builtin factory first, so the property only causes a re-entrant
// double instantiation ("Serial filter factory has previously been instantiated") which aborts ghidra init.
// The ghidra:// URL protocol handler below is also looked up through the system class loader, and sbt 2 by
// default launches forked test workers with only the worker jars on java.class.path (the real classpath lives in
// a child URLClassLoader). The Raw layering strategy instead forks tests with the full classpath on the system
// class loader.
javaOptions := Seq("-Djava.protocol.handler.pkgs=ghidra.framework.protocol")
Test / classLoaderLayeringStrategy := ClassLoaderLayeringStrategy.Raw
