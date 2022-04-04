name := "fuzzyc2cpg"

dependsOn(Projects.semanticcpg, Projects.dataflowengineoss % Test, Projects.x2cpg % "compile->compile;test->test")

libraryDependencies ++= Seq(
  "org.antlr"                % "antlr4-runtime"             % Versions.antlr,
  "org.apache.commons"       % "commons-lang3"              % "3.12.0",
  "commons-cli"              % "commons-cli"                % "1.5.0",
  "com.github.pathikrit"    %% "better-files"               % "3.9.1",
  "org.scala-lang.modules"  %% "scala-parallel-collections" % "1.0.4",
  "org.apache.logging.log4j" % "log4j-slf4j-impl"           % Versions.log4j     % Runtime,
  "com.github.sbt"           % "junit-interface"            % "0.13.3"           % Test,
  "junit"                    % "junit"                      % "4.13.2"           % Test,
  "org.scalatest"           %% "scalatest"                  % Versions.scalatest % Test
)

scalacOptions -= "-Xfatal-warnings" // some antl-generated sources prompt compiler warnings :(

scalacOptions ++= Seq(
  "-deprecation", // Emit warning and location for usages of deprecated APIs.
  "-encoding",
  "utf-8",                  // Specify character encoding used by source files.
  "-explaintypes",          // Explain type errors in more detail.
  "-feature",               // Emit warning and location for usages of features that should be imported explicitly.
  "-language:existentials", // Existential types (besides wildcard types) can be written and inferred
  "-language:experimental.macros", // Allow macro definition (besides implementation and application)
  "-language:higherKinds",         // Allow higher-kinded types
  "-language:implicitConversions", // Allow definition of implicit functions called views
  "-unchecked",                    // Enable additional warnings where generated code depends on assumptions.
  "-Xcheckinit",                   // Wrap field accessors to throw an exception on uninitialized access.
  // "-Xfatal-warnings",                  // Fail the compilation if there are any warnings.
  "-Xlint:adapted-args",           // Warn if an argument list is modified to match the receiver.
  "-Xlint:constant",               // Evaluation of a constant arithmetic expression results in an error.
  "-Xlint:delayedinit-select",     // Selecting member of DelayedInit.
  "-Xlint:doc-detached",           // A Scaladoc comment appears to be detached from its element.
  "-Xlint:inaccessible",           // Warn about inaccessible types in method signatures.
  "-Xlint:infer-any",              // Warn when a type argument is inferred to be `Any`.
  "-Xlint:missing-interpolator",   // A string literal appears to be missing an interpolator id.
  "-Xlint:option-implicit",        // Option.apply used implicit view.
  "-Xlint:package-object-classes", // Class or object defined in package object.
  "-Xlint:poly-implicit-overload", // Parameterized overloaded implicit methods are not visible as view bounds.
  "-Xlint:private-shadow",         // A private field (or class parameter) shadows a superclass field.
  "-Xlint:stars-align",            // Pattern sequence wildcard must align with sequence component.
  "-Xlint:type-parameter-shadow",  // A local type parameter shadows a type already in scope.
  "-Ywarn-dead-code",              // Warn when dead code is identified.
  "-Ywarn-extra-implicit",         // Warn when more than one implicit parameter section is defined.
  "-Xlint:nullary-unit",           // Warn when nullary methods return Unit.
  "-Ywarn-numeric-widen",          // Warn when numerics are widened.
  "-Ywarn-unused:implicits",       // Warn if an implicit parameter is unused.
  "-Ywarn-unused:imports",         // Warn if an import selector is not referenced.
  "-Ywarn-unused:locals",          // Warn if a local definition is unused.
  "-Ywarn-unused:params",          // Warn if a value parameter is unused.
  "-Ywarn-unused:patvars",         // Warn if a variable bound in a pattern is unused.
  "-Ywarn-unused:privates"         // Warn if a private member is unused.
  // "-Ywarn-value-discard"               // Warn when non-Unit expression results are unused.
)

compile / javacOptions ++= Seq("-Xlint:all", "-Xlint:-cast", "-g", "--release", "8")

Test / fork := true
testOptions += Tests.Argument(TestFrameworks.JUnit, "-a", "-v")

enablePlugins(Antlr4Plugin)
Antlr4 / antlr4PackageName := Some("io.joern.fuzzyc2cpg")
Antlr4 / antlr4Version     := Versions.antlr
Antlr4 / javaSource        := (Compile / sourceManaged).value

enablePlugins(JavaAppPackaging)

Universal / packageName       := name.value
Universal / topLevelDirectory := None

Test / packageBin / publishArtifact := true
