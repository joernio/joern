name               := "c2cpg"
scalaVersion       := "2.13.8"
crossScalaVersions := Seq("2.13.8", "3.3.0")

dependsOn(Projects.semanticcpg, Projects.dataflowengineoss % Test, Projects.x2cpg % "compile->compile;test->test")

libraryDependencies ++= Seq(
  "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.4",
  "com.diffplug.spotless"   % "spotless-eclipse-cdt"       % "10.5.0",
  "org.jline"               % "jline"                      % "3.23.0",
  "org.scalatest"          %% "scalatest"                  % Versions.scalatest % Test
)

dependencyOverrides ++= Seq(
  /* tl;dr; we'll stay on 2.19.0
   * Full story: if we upgrade to 2.20.0 we run into the following osgi error:
   *   Unknown error checking OSGI environment.
   *   java.lang.reflect.InvocationTargetException
   *     at java.base/jdk.internal.reflect.NativeMethodAccessorImpl.invoke0(Native Method)
   *     at java.base/jdk.internal.reflect.NativeMethodAccessorImpl.invoke(NativeMethodAccessorImpl.java:77)
   *     at java.base/jdk.internal.reflect.DelegatingMethodAccessorImpl.invoke(DelegatingMethodAccessorImpl.java:43)
   *     at java.base/java.lang.reflect.Method.invoke(Method.java:568)
   *     at org.apache.logging.log4j.util.OsgiServiceLocator.checkOsgiAvailable(OsgiServiceLocator.java:39)
   *   ...
   *   Caused by: java.lang.NullPointerException: Cannot invoke "org.osgi.framework.BundleContext.getBundles()" because "context" is null
   *     at com.diffplug.spotless.extra.eclipse.base.osgi.SimpleBundle.<init>(SimpleBundle.java:57)
   *     at com.diffplug.spotless.extra.eclipse.base.osgi.SimpleBundle.<init>(SimpleBundle.java:49)
   *     at com.diffplug.spotless.extra.eclipse.base.osgi.FrameworkBundleRegistry.getBundle(FrameworkBundleRegistry.java:47)
   *     at org.osgi.framework.FrameworkUtil.lambda$5(FrameworkUtil.java:234)
   */
  "org.apache.logging.log4j" % "log4j-core"        % "2.19.0" % Optional,
  "org.apache.logging.log4j" % "log4j-slf4j2-impl" % "2.19.0" % Optional
)

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

enablePlugins(JavaAppPackaging, LauncherJarPlugin)

Universal / packageName       := name.value
Universal / topLevelDirectory := None
