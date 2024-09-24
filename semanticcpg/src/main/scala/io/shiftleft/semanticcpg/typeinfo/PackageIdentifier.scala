package io.shiftleft.semanticcpg.typeinfo

import io.shiftleft.semanticcpg.typeinfo.LanguageFrontend.Java

enum LanguageFrontend {
  case Java
  override def toString: String = this match
    case Java => "java"
    case _ => throw new RuntimeException(s"no toString override for LanguageFrontend enum constant value")
}

/** Type representing all info needed by the fetcher to download
 * and identify type info for a package
 */
final case class PackageIdentifier(lang: LanguageFrontend, name: String)
