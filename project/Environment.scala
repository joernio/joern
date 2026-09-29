import sbt.*

object Environment {

  object OperatingSystemType extends Enumeration {
    type OperatingSystemType = Value

    val Windows, Linux, Mac, Unknown = Value
  }

  object ArchitectureType extends Enumeration {
    type ArchitectureType = Value

    val X86, ARMv8 = Value
  }

  lazy val operatingSystem: OperatingSystemType.OperatingSystemType =
    if (scala.util.Properties.isMac) OperatingSystemType.Mac
    else if (scala.util.Properties.isLinux) OperatingSystemType.Linux
    else if (scala.util.Properties.isWin) OperatingSystemType.Windows
    else OperatingSystemType.Unknown

  lazy val architecture: ArchitectureType.ArchitectureType =
    if (scala.util.Properties.propOrNone("os.arch").contains("aarch64")) ArchitectureType.ARMv8
    // We do not distinguish between x86 and x64. E.g, a 64 bit Windows will always lie about
    // this and will report x86 anyway for backwards compatibility with 32 bit software.
    else ArchitectureType.X86

}

/** Exposes the build machine's OS/architecture as sbt settings. Settings are re-evaluated on every build load,
  * so values derived from them (e.g. the platform-specific binary names that cached download tasks take as
  * inputs) participate in sbt's cache keys. Reading `Environment.operatingSystem` directly inside a task
  * instead would be a hidden environment read that the cache key cannot see.
  */
object BuildEnvironmentPlugin extends AutoPlugin {
  override def trigger = allRequirements

  object autoImport {
    lazy val buildOperatingSystem =
      settingKey[Environment.OperatingSystemType.OperatingSystemType]("operating system of the machine running the build")
    lazy val buildArchitecture =
      settingKey[Environment.ArchitectureType.ArchitectureType]("CPU architecture of the machine running the build")
  }

  override def globalSettings = {
    import autoImport.*
    Seq(
      buildOperatingSystem := Environment.operatingSystem,
      buildArchitecture := Environment.architecture
    )
  }
}
