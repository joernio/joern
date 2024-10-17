package io.shiftleft.semanticcpg.typeinfo.version

import io.shiftleft.semanticcpg.typeinfo.version.Version
import com.github.zafarkhaja.semver.{Version => SVer}

case class SemVer2(str: String) extends Version(str) {
  private val parsedSemVer: SVer = SVer.parse(rawVersionString)

  override def toFetcherStr: String = rawVersionString

  override def toString: String = rawVersionString

  override def compare(other: Version): Int = {
    other match {
      // Don't consider build metadata like git commit hashes in the comparison.
      case otherSemVer: SemVer2 => parsedSemVer.compareToIgnoreBuildMetadata(otherSemVer.parsedSemVer)
      case _                    => throw new IllegalArgumentException(s"$other is not a SemVer2")
    }
  }

  override def equals(other: Any): Boolean = {
    other match
      case o: SemVer2 => parsedSemVer.equals(o.parsedSemVer)
      case _          => false
  }
}
