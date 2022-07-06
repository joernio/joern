name := "console"

enablePlugins(JavaAppPackaging)

val ScoptVersion          = "4.0.1"
val BetterFilesVersion    = "3.9.1"
val CaskVersion           = "0.8.3"
val CirceVersion          = "0.14.2"
val AmmoniteVersion       = "2.5.3"
val ZeroturnaroundVersion = "1.15"

dependsOn(
  Projects.semanticcpg,
  Projects.macros,
  Projects.c2cpg      % Test,
  Projects.x2cpg      % "compile->compile;test->test"
)

libraryDependencies ++= Seq(
  "io.shiftleft"         %% "codepropertygraph" % Versions.cpg,
  "com.github.scopt"     %% "scopt"             % ScoptVersion,
  "com.github.pathikrit" %% "better-files"      % BetterFilesVersion,
  "org.typelevel"        %% "cats-effect"       % Versions.cats,
  "io.circe"             %% "circe-generic"     % CirceVersion,
  "io.circe"             %% "circe-parser"      % CirceVersion,
  "org.zeroturnaround"    % "zt-zip"            % ZeroturnaroundVersion,
  "com.lihaoyi"          %% "ammonite"          % AmmoniteVersion cross CrossVersion.full,
  "com.lihaoyi"          %% "os-lib"            % "0.8.1",
  "com.lihaoyi"          %% "cask"              % CaskVersion,
  "org.scalatest"        %% "scalatest"         % Versions.scalatest % Test
)

Test / packageBin / publishArtifact := true

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
  "-Ywarn-numeric-widen",          // Warn when numerics are widened.
  "-Ywarn-unused:implicits",       // Warn if an implicit parameter is unused.
  "-Ywarn-unused:imports",         // Warn if an import selector is not referenced.
  "-Ywarn-unused:locals",          // Warn if a local definition is unused.
  "-Ywarn-unused:params",          // Warn if a value parameter is unused.
  "-Ywarn-unused:patvars",         // Warn if a variable bound in a pattern is unused.
  "-Ywarn-unused:privates",        // Warn if a private member is unused.
  "-Yrangepos"
)

// would love to reenable, but somehow StorageBackend.scala triggers a strange `[warn] method with a single empty parameter list overrides method without any parameter list` that doesn't make sense to me...
scalacOptions -= "-Xfatal-warnings"
