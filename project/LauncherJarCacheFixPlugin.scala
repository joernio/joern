import sbt.*
import sbt.Keys.*
import com.typesafe.sbt.packager.archetypes.JavaAppPackaging
import com.typesafe.sbt.packager.archetypes.JavaAppPackaging.autoImport.scriptClasspath
import com.typesafe.sbt.packager.archetypes.jar.LauncherJarPlugin
import com.typesafe.sbt.packager.archetypes.jar.LauncherJarPlugin.autoImport.packageJavaLauncherJar

/** Works around an sbt 2.x task-cache issue: `packageJavaLauncherJar / packageOptions` is restored from the
  * disk cache with a stale `Class-Path` manifest attribute even after `version` changes (which happens on every
  * commit and every dirty working tree with sbt-dynver). The resulting launcher jar then references jar file
  * names that do not exist in the staged `lib/` directory, and the frontend fails to start with
  * `ClassNotFoundException`. Re-defining the task as uncached forces the manifest to always be computed from the
  * current `scriptClasspath`.
  */
object LauncherJarCacheFixPlugin extends AutoPlugin {
  override def requires = LauncherJarPlugin
  override def trigger  = allRequirements

  override def projectSettings: Seq[Setting[?]] = Seq(
    packageJavaLauncherJar / packageOptions := Def.uncached {
      val classpath    = (packageJavaLauncherJar / scriptClasspath).value
      val mainClassOpt = (Compile / packageJavaLauncherJar / mainClass).value
      val attrs        = mainClassOpt.toSeq.map("Main-Class" -> _) :+ ("Class-Path" -> classpath.mkString(" "))
      Seq(Pkg.ManifestAttributes(attrs*))
    }
  )
}
