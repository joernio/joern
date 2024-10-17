package io.shiftleft.semanticcpg.typeinfo.version

/** This should be overridden for each version-format where each deriving class provides functionality to convert
  * between literal version strings to strings usable by the fetcher. Should also provide a compare function that
  * returns -1 for this < other, 0 this == other, 1 otherwise. The compare method is used to resolve dependencies with
  * <,=,>,etc constraints.
  */
abstract class Version(protected val rawVersionString: String) {
  def compare(other: Version): Int
  def toFetcherStr: String
}
