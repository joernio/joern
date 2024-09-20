package io.shiftleft.semanticcpg.typeinfo

// Languages should override this providing conversions
// from version strings to strings usable by the fetcher.
// Should also provide a compare function that returns
// -1 for this < other, 0 this == other, 1 otherwise.
abstract class Version(str: String):
  def compare(other: Version): Int
  def toFetcherStr: String
