name               := "x2cpg"
scalaVersion       := "2.13.8"
crossScalaVersions := Seq("2.13.8", "3.1.2")

dependsOn(Projects.semanticcpg)

libraryDependencies ++= Seq(
  "org.slf4j"                % "slf4j-api"          % "1.7.36",
  "org.apache.logging.log4j" % "log4j-slf4j-impl"   % Versions.log4j         % Optional,
  "org.gradle"               % "gradle-tooling-api" % Versions.gradleTooling % Optional,
  "org.scalatest"           %% "scalatest"          % Versions.scalatest     % Test
)

Test / packageBin / publishArtifact := true

Compile / doc / scalacOptions ++= Seq("-doc-title", "semanticcpg apidocs", "-doc-version", version.value)

scalacOptions ++= Seq() ++ (
  CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((3, _)) => Seq()
    case _ =>
      Seq(
        "-deprecation", // Emit warning and location for usages of deprecated APIs.
        "-encoding",
        "utf-8",         // Specify character encoding used by source files.
        "-explaintypes", // Explain type errors in more detail.
        "-feature",      // Emit warning and location for usages of features that should be imported explicitly.
        "-language:existentials",        // Existential types (besides wildcard types) can be written and inferred
        "-language:experimental.macros", // Allow macro definition (besides implementation and application)
        "-language:higherKinds",         // Allow higher-kinded types
        "-language:implicitConversions", // Allow definition of implicit functions called views
        "-unchecked",                    // Enable additional warnings where generated code depends on assumptions.
        "-Xcheckinit",                   // Wrap field accessors to throw an exception on uninitialized access.
        // "-Xfatal-warnings",            //-Werror is incompatible with the concept of @deprecate.
        // TODO: Find the right incantation to ensure that deprecation warnings are
        //       not suppressed but are not treated as error either
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
      )
  }
)

compile / javacOptions ++= Seq("-Xlint:all", "-Xlint:-cast", "-g")
Test / fork := true

enablePlugins(JavaAppPackaging)

Universal / packageName       := name.value
Universal / topLevelDirectory := None
