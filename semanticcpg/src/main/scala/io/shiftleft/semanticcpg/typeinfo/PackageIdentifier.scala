package io.shiftleft.semanticcpg.typeinfo

enum LanguageFrontend {
  case Java
  override def toString: String = this match
    case Java => "java"
    case _ => throw new RuntimeException(s"no toString override for LanguageFrontend enum constant value")
}

object LanguageFrontend {
  def ofString(str: String): LanguageFrontend = str match
    case "java" => LanguageFrontend.Java
    case _ => throw new RuntimeException(s"couldn't convert string to LanguageFrontend: $str")
}

/** Type representing all info needed by the fetcher to download
 * and identify type info for a package
 */
final case class PackageIdentifier(lang: LanguageFrontend, name: String)
