name := "c2cpg"

dependsOn(
  Projects.semanticcpg,
  Projects.dataflowengineoss % "compile->compile;test->test",
  Projects.x2cpg             % "compile->compile;test->test"
)

libraryDependencies ++= Seq(
  "org.scala-lang.modules" %% "scala-parallel-collections" % Versions.scalaParallel,
  "org.eclipse.platform"    % "org.eclipse.core.resources" % Versions.eclipseCore,
  "org.eclipse.platform"    % "org.eclipse.text"           % Versions.eclipseText,
  // see note in readme re self-publishing cdt-core
  "io.joern"       % "eclipse-cdt-core" % Versions.eclipseCdt,
  "org.scalatest" %% "scalatest"        % Versions.scalatest % Test
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

compile / javacOptions ++= Seq("-Xlint:all", "-Xlint:-cast", "-g")
Test / fork := true

enablePlugins(JavaAppPackaging, LauncherJarPlugin)

Universal / packageName       := name.value
Universal / topLevelDirectory := None

githubOwner      := "Privado-Inc"
githubRepository := "joern"
credentials +=
  Credentials(
    "GitHub Package Registry",
    "maven.pkg.github.com",
    "Privado-Inc",
    sys.env.getOrElse("GITHUB_TOKEN", "N/A")
  )
