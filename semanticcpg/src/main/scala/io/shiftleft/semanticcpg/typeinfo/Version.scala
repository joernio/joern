package io.shiftleft.semanticcpg.typeinfo

/** This should be overridden for each version-format where each deriving class provides functionality to convert
 * between literal version strings to strings usable by the fetcher. Should also provide a compare function that 
 * returns -1 for this < other, 0 this == other, 1 otherwise. The compare method is used to resolve dependencies 
 * with <,=,>,etc constraints.
 */
abstract class Version(val str: String) {
  def compare(other: Version): Int = {
    str.compare(other.str)
  }
  
  def toFetcherStr: String = str
  
  override def toString: String = str

  override def equals(obj: Any): Boolean = {
    obj match
      case obj: Version => str == obj.str
      case _ => false
  }
}

/** This class implements raw version strings, a thing wrapper around String otherwise. */
final class RawVersion(str: String) extends Version(str) {
  
}

class SemVer(val major: Int, val minor: Int, val patch: Int) 
  extends Version(s"$major.$minor.$patch") {

  override def compare(other: Version): Int = {
    if (!other.isInstanceOf[SemVer]) {
      throw new RuntimeException("Can't compare non-SemVer with SemVer")
    }
    val otherSemVer: SemVer = other.asInstanceOf[SemVer]
    (major.compare(otherSemVer.major), minor.compare(otherSemVer.minor), patch.compare(otherSemVer.patch)) match
      case (-1, _, _) | (0, -1, _) | (0, 0, -1) => -1
      case (1, _, _) | (0, 1, _) | (0, 0, 1)    => 1
      case (0, 0, 0)                            => 0
  }

  override def equals(other: Any): Boolean = {
    other match
      case o: SemVer => major == o.major && minor == o.minor && patch == o.patch
      case _ => false
  }
}
