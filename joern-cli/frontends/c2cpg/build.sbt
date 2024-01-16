import java.io.FileInputStream
import java.io.FileOutputStream
import java.util.jar.JarEntry
import java.util.jar.JarInputStream
import java.util.jar.JarOutputStream

name := "c2cpg"

dependsOn(
  Projects.semanticcpg,
  Projects.dataflowengineoss % "compile->compile;test->test",
  Projects.x2cpg             % "compile->compile;test->test"
)

lazy val cdtCoreDepVersion        = "8.4.0.202401051815"
lazy val cdtCoreDepName           = s"org.eclipse.cdt.core"
lazy val cdtCoreDepNameAndVersion = s"org.eclipse.cdt.core_$cdtCoreDepVersion"
lazy val cdtCodeDepUrl =
  s"https://ci.eclipse.org/cdt/job/cdt/job/main/347/artifact/releng/org.eclipse.cdt.repo/target/repository/plugins/$cdtCoreDepNameAndVersion.jar"

libraryDependencies ++= Seq(
  "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.4",
  "org.eclipse.platform"    % "org.eclipse.core.resources" % "3.20.0",
  "org.eclipse.platform"    % "org.eclipse.text"           % "3.13.100",
  "org.eclipse.platform"    % "org.eclipse.cdt.core"       % cdtCoreDepVersion  % Provided from cdtCodeDepUrl,
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

compile / javacOptions ++= Seq("-Xlint:all", "-Xlint:-cast", "-g")
Test / fork := true

lazy val signingFiles = List("META-INF/ECLIPSE_.RSA", "META-INF/ECLIPSE_.SF")

/* Dirty hack: we access cdt-internal types which are package-private. In order to do so,
 * `MacroArgumentExtractor` from this repo is in the `org.eclipse.cdt.internal.core.parser.scanner` package.
 * The cdt jar is signed to ensure that doesn't happen, but because we're stubborn and yolo we simply remove the signing files.
 */
lazy val removeSigningInfo = taskKey[Unit]("Remove signing info from Eclipse CDT jar file")
removeSigningInfo := {
  import java.nio.file.Files
  val log = streams.value.log
  val managedClasspathValue = (Compile / managedClasspath).value
  val lib = unmanagedBase.value
  if (!lib.exists) IO.createDirectory(lib)
  val outputFile = lib / s"$cdtCoreDepNameAndVersion.custom.jar"
  if (!outputFile.exists) {
    managedClasspathValue.find(_.data.name.contains(cdtCoreDepName)) match {
      case Some(path) =>
        val jarPath    = path.data.absolutePath
        val inputFile  = new File(jarPath)

        try {
          val jarInputStream  = new JarInputStream(new FileInputStream(inputFile))
          val jarOutputStream = new JarOutputStream(new FileOutputStream(outputFile))

          Iterator.continually(jarInputStream.getNextJarEntry).takeWhile(_ != null).foreach { entry =>
            val entryName = entry.getName
            if (!signingFiles.contains(entryName)) {
              jarOutputStream.putNextEntry(new JarEntry(entryName))
              Iterator.continually(jarInputStream.read()).takeWhile(_ != -1).foreach(jarOutputStream.write)
            }
            jarOutputStream.closeEntry()
          }

          jarInputStream.close()
          jarOutputStream.close()

          log.info("Removed signing info from Eclipse CDT jar file")

          // cleanup other versions of the Eclipse CDT jar file, if any
          lib.listFiles.foreach { file =>
            if (!file.name.contains(cdtCoreDepNameAndVersion) && file.name.contains(cdtCoreDepName)) {
              file.delete()
            }
          }
        } catch {
          case e: Exception => log.error(s"Error removing signing info from '$jarPath': ${e.getMessage}")
        }
      case _ => // do nothing
    }
  }
}

lazy val removeSigningInfoStartup: State => State = { s: State => "removeSigningInfo" :: s }
Global / onLoad := {
  val old = (Global / onLoad).value
  removeSigningInfoStartup compose old
}

enablePlugins(JavaAppPackaging, LauncherJarPlugin)

Universal / packageName       := name.value
Universal / topLevelDirectory := None
