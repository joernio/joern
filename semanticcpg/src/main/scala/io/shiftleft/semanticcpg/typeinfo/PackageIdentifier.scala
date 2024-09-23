package io.shiftleft.semanticcpg.typeinfo

import io.shiftleft.semanticcpg.typeinfo.LanguageFrontend.Java

enum LanguageFrontend:
  case Java

// Type representing all info needed by the fetcher to download
// and identify type info for a package
//
// Languages should override this providing conversions
// from fully-qualified package names as strings to strings
// representing paths usable by the fetcher. An example:
// JavaPackageName("com.google.guava.guava-parent").toFetcherStr ==
//   "com/google/guava/guava-parent
// and this might be different for Go and python
abstract class PackageIdentifier(name: String):
  val lang: LanguageFrontend 
  def toFetcherStr: String

object PackageIdentifier:
  def langToString(lang: LanguageFrontend): String =
    lang match
      case Java => "java"