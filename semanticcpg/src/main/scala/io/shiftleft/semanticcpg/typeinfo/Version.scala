package io.shiftleft.semanticcpg.typeinfo

/** Languages should override this providing conversions from version strings to strings usable by the fetcher. Should
  * also provide a compare function that returns -1 for this < other, 0 this == other, 1 otherwise. The compare method
  * is used to resolve dependencies with <,=,>,etc constraints.
  */
abstract class Version(str: String) {
  def compare(other: Version): Int
  def toFetcherStr: String
}

class SemVer(str: String) extends Version(str) {
  val nums: Array[Int] = str.split("\\.").map(_.toInt)
  val major: Int       = nums(0)
  val minor: Int       = nums(1)
  val patch: Int       = nums(2)

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
      case o: SemVer =>
        this.isInstanceOf[SemVer] &&
          major == o.major &&
          minor == o.minor &&
          patch == o.patch
      case _ => false
  }

  override def toFetcherStr: String = str

  override def toString: String = str
}
