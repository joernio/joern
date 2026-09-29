import sbt.*

import versionsort.VersionHelper

import java.io.File
import java.net.URI
import java.nio.file.Files
import scala.sys.process.stringToProcess
import scala.util.Try

/** Helpers for downloading external artifacts (astgen binaries, parser phars, type stubs, ...).
  *
  * A task whose body is [[downloadArtifacts]] is cached by sbt: the cache key is derived from the task's inputs
  * (url, file names, version settings, ...), and the downloaded files are declared as outputs via
  * `Def.declareOutput`, so on a cache hit sbt re-materializes them from the disk cache without any network
  * access. Note that declared outputs must live inside the project output directory (`target.value`); use
  * [[stageArtifacts]] (from an uncached task) to sync them to their final destinations, e.g. `bin/astgen` in the
  * project base directory (runtime lookup for local runs and tests) and the Universal staging directory (for the
  * distribution).
  */
object DownloadHelper {

  /** Reads `configKey` from the `application.conf` in the given resource directory, e.g. the version of an
    * external artifact to download.
    */
  def appConfigVersion(resourceDirectory: File, configKey: String): String =
    com.typesafe.config.ConfigFactory
      .parseFile(resourceDirectory / "application.conf")
      .resolve()
      .getString(configKey)

  /** Probes whether `binaryName` is available on the system PATH and reports at least `requiredVersion`.
    * Meant to be called from a setting, so that the probe result is re-evaluated on every build load and
    * participates in the cache key of downstream cached tasks.
    */
  def hasCompatibleVersionOnPath(
    binaryName: String,
    requiredVersion: String,
    versionFlag: String = "--version",
    versionPrefix: String = ""
  ): Boolean = {
    Try(s"$binaryName $versionFlag".!!).toOption.map(_.strip().stripPrefix(versionPrefix).strip) match {
      case Some(installedVersion) if installedVersion.nonEmpty && installedVersion != "unknown" =>
        VersionHelper.compare(installedVersion, requiredVersion) >= 0
      case _ => false
    }
  }

  /** Selects the released binary name(s) for the given platform — pass `buildOperatingSystem.value` and
    * `buildArchitecture.value` so the selection becomes visible to the sbt cache. Variants that a release does
    * not provide should be left as `None`; the architecture-specific variant falls back to the generic one, and
    * on unknown platforms all provided variants are returned.
    */
  def platformBinaries(
    os: Environment.OperatingSystemType.OperatingSystemType,
    arch: Environment.ArchitectureType.ArchitectureType
  )(
    windows: Option[String] = None,
    windowsArm: Option[String] = None,
    linux: Option[String] = None,
    linuxArm: Option[String] = None,
    mac: Option[String] = None,
    macArm: Option[String] = None
  ): Seq[String] = {
    import Environment.{ArchitectureType => Arch, OperatingSystemType => OS}
    def pick(primary: Option[String], fallback: Option[String]): Seq[String] = primary.orElse(fallback).toSeq
    (os, arch) match {
      case (OS.Windows, Arch.ARMv8) => pick(windowsArm, windows)
      case (OS.Windows, _)          => pick(windows, windowsArm)
      case (OS.Linux, Arch.ARMv8)   => pick(linuxArm, linux)
      case (OS.Linux, _)            => pick(linux, linuxArm)
      case (OS.Mac, Arch.ARMv8)     => pick(macArm, mac)
      case (OS.Mac, _)              => pick(mac, macArm)
      case _ => Seq(windows, windowsArm, linux, linuxArm, mac, macArm).flatten
    }
  }

  /** Body of a cached download task: downloads `baseUrl + fileName` for each of `fileNames` into `downloadDir`
    * (which must be inside the project output directory, e.g. `target.value / "astgen-download"`).
    *
    * The returned refs still need to be declared as task outputs at the call site, so that sbt caches and
    * re-materializes them on cache hits (`Def.declareOutput` may only be called within a task macro, which is
    * why it cannot happen here):
    * {{{
    *   someDownloadTask := DownloadHelper
    *     .downloadArtifacts(target.value / "some-download", someDlUrl.value, someFileNames.value, fileConverter.value)
    *     .map { vf => Def.declareOutput(vf); vf }
    * }}}
    */
  def downloadArtifacts(
    downloadDir: File,
    baseUrl: String,
    fileNames: Seq[String],
    conv: xsbti.FileConverter,
    executable: Boolean = true
  ): Seq[xsbti.VirtualFile] = {
    IO.createDirectory(downloadDir)
    fileNames.map { fileName =>
      val file = downloadDir / fileName
      println(s"[INFO] downloading $baseUrl$fileName to $file")
      UrlRetry.downloadWithRetries(new URI(s"$baseUrl$fileName").toURL, file)
      // permissions are lost during the download; need to set them manually
      if (executable) file.setExecutable(true, false)
      conv.toVirtualFile(file.toPath)
    }
  }

  def copyIfChanged(src: File, tgt: File, executable: Boolean): Unit = {
    if (!tgt.isFile || (executable && !tgt.canExecute) || Files.mismatch(src.toPath, tgt.toPath) != -1L) {
      IO.copyFile(src, tgt)
      if (executable) tgt.setExecutable(true, false)
    }
  }

  /** Body of an uncached stage task: syncs the given downloaded artifacts (e.g. the result of a cached
    * [[downloadArtifacts]] task) into each of the destination directories, copying only on change.
    */
  def stageArtifacts(
    artifacts: Seq[xsbti.HashedVirtualFileRef],
    conv: xsbti.FileConverter,
    destinations: Seq[File],
    executable: Boolean = true
  ): Unit = {
    destinations.foreach(_.mkdirs())
    for {
      ref <- artifacts
      src = conv.toPath(ref).toFile
      dir <- destinations
    } copyIfChanged(src, dir / src.getName, executable)
  }
}
