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
  
class JavaPackageIdentifier(name: String) extends PackageIdentifier(name) {
  override val lang: LanguageFrontend = Java
  override def toFetcherStr: String = name.replace('.', '/')
  override def toString: String = toFetcherStr
}

object PackageIdentifier:
  def langToString(lang: LanguageFrontend): String =
    lang match
      case Java => "java"