object Environment {

  object OperatingSystemType extends Enumeration {
    type OperatingSystem = Value

    val Windows, Linux, Mac, Unknown = Value
  }

  object ArchitectureType extends Enumeration {
    type Architecture = Value

    val X86, ARM = Value
  }

  val OperatingSystem: OperatingSystemType.OperatingSystem =
    if (scala.util.Properties.isMac) OperatingSystemType.Mac
    else if (scala.util.Properties.isLinux) OperatingSystemType.Linux
    else if (scala.util.Properties.isWin) OperatingSystemType.Windows
    else OperatingSystemType.Unknown

  val Architecture: ArchitectureType.Architecture =
    if (scala.util.Properties.propOrNone("os.arch").contains("aarch64")) ArchitectureType.ARM
    // We do not distinguish between x86 and x64. E.g, a 64 bit Windows will always lie about
    // this and will report x86 anyway for backwards compatibility with 32 bit software.
    else ArchitectureType.X86

}
