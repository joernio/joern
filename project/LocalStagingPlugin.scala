import sbt.*
import sbt.Keys.baseDirectory
import com.typesafe.sbt.packager.universal.UniversalPlugin
import com.typesafe.sbt.packager.universal.UniversalPlugin.autoImport.*

/** Forces every project that uses UniversalPlugin (i.e. JavaAppPackaging) to write its stage output into the
  * subproject-local target/universal/stage/ directory rather than SBT 2.x's centralised
  * target/out/jvm/<scala>/<project>/ tree. This keeps all existing symlinks working.
  */
object LocalStagingPlugin extends AutoPlugin {
  override def requires = UniversalPlugin
  override def trigger  = allRequirements

  override def projectSettings: Seq[Setting[?]] = Seq(
    Universal / stagingDirectory := baseDirectory.value / "target" / "universal" / "stage"
  )
}
