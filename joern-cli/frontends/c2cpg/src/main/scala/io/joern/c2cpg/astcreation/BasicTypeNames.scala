package io.joern.c2cpg.astcreation

import org.eclipse.cdt.core.dom.ast.cpp.{ICPPASTSimpleDeclSpecifier, ICPPBasicType}
import org.eclipse.cdt.core.dom.ast.{IASTSimpleDeclSpecifier, IBasicType, IPointerType, IQualifierType, IType, ITypedef}

import scala.annotation.nowarn

/** One valid C/C++ name per basic type, however it is spelled in the source.
  *
  * CDT renders the same type differently depending on where a string comes from: `unsigned short int` is `short
  * unsigned int` for a declaration specifier, `unsigned short int` for an expression and `unsigned short` in the
  * source. Queries match on exact names (`.typeFullName("long")`), so all of them get the same name:
  *   - `int` is dropped from `long int`, `short int` and so on.
  *   - The order of the keywords does not matter: `long unsigned` is `unsigned long`.
  *   - An explicit `signed` is dropped: `signed int` is the same type as `int`, and for bit-fields the signedness of a
  *     plain `int` is implementation-defined, so like most implementations (e.g. GCC) we define it as signed.
  *     `signed char` stays: `char`, `signed char` and `unsigned char` are three different types.
  *   - `long long` and `long double` keep their space, to be valid C.
  */
object BasicTypeNames {

  private val Keyword =
    "(?:unsigned|signed|short|long|int|char|double|float|void|bool|_Bool|__int128|_Complex|_Imaginary|wchar_t|char8_t|char16_t|char32_t)"

  /** A maximal run of basic type keywords, e.g. `unsigned long int`. */
  private val KeywordRun = s"\\b$Keyword(?:\\s+$Keyword)*\\b".r

  private val KeywordsFollowedBySpace = Set("signed", "unsigned", "volatile")

  /** Replaces every run of basic type keywords in `rawType` with its canonical name. */
  def normalize(rawType: String): String = {
    KeywordRun.replaceAllIn(rawType, found => java.util.regex.Matcher.quoteReplacement(canonicalName(found.matched)))
  }

  /** The canonical name of one run of basic type keywords, e.g. `long signed int` is `long`. */
  def canonicalName(keywordRun: String): String = {
    val keywords  = keywordRun.split("\\s+").toList
    val longCount = keywords.count(_ == "long")
    val base      =
      if (keywords.contains("__int128")) "__int128"
      else if (keywords.contains("wchar_t")) "wchar_t"
      else if (keywords.contains("char8_t")) "char8_t"
      else if (keywords.contains("char16_t")) "char16_t"
      else if (keywords.contains("char32_t")) "char32_t"
      else if (keywords.contains("_Bool")) "_Bool"
      else if (keywords.contains("bool")) "bool"
      else if (keywords.contains("float")) "float"
      else if (keywords.contains("void")) "void"
      else if (keywords.contains("char")) "char"
      else if (keywords.contains("double")) { if (longCount > 0) "long double" else "double" }
      else if (keywords.contains("short")) "short"
      else {
        longCount match {
          case 0 => "int"
          case 1 => "long"
          case _ => "long long"
        }
      }
    val withSign =
      if (keywords.contains("unsigned")) s"unsigned $base"
      else if (keywords.contains("signed") && base == "char") s"signed $base"
      else base
    if (keywords.contains("_Complex")) s"$withSign _Complex"
    else if (keywords.contains("_Imaginary")) s"$withSign _Imaginary"
    else withSign
  }

  /** Whether whitespace between the neighbouring tokens `left` and `right` of a type name is part of the name. */
  def keepsWhitespaceBetween(left: String, right: String): Boolean = {
    KeywordsFollowedBySpace.contains(left) ||
    KeywordsFollowedBySpace.contains(right) ||
    (left == "long" && (right == "long" || right == "double")) ||
    (right == "_Complex" || right == "_Imaginary")
  }

  /** The name of a basic type, from its kind and modifiers instead of a rendered string. `None` for the kinds where
    * CDT's rendered string is kept.
    */
  def forBasicType(tpe: IBasicType): Option[String] = {
    val base = tpe.getKind match {
      case IBasicType.Kind.eChar    => Some("char")
      case IBasicType.Kind.eWChar   => Some("wchar_t")
      case IBasicType.Kind.eChar8   => Some("char8_t")
      case IBasicType.Kind.eChar16  => Some("char16_t")
      case IBasicType.Kind.eChar32  => Some("char32_t")
      case IBasicType.Kind.eBoolean => Some(if (tpe.isInstanceOf[ICPPBasicType]) "bool" else "_Bool")
      case IBasicType.Kind.eInt128  => Some("__int128")
      case IBasicType.Kind.eVoid    => Some("void")
      case IBasicType.Kind.eFloat   => Some("float")
      case IBasicType.Kind.eDouble  => Some(if (tpe.isLong) "long double" else "double")
      case IBasicType.Kind.eInt     =>
        Some(if (tpe.isShort) "short" else if (tpe.isLongLong) "long long" else if (tpe.isLong) "long" else "int")
      case _ => None
    }
    base.map(withSignAndComplex(tpe.isUnsigned, tpe.isSigned, tpe.isComplex, tpe.isImaginary, _))
  }

  /** The name of a simple declaration specifier (a builtin type keyword run in a declaration), from its kind and
    * modifiers. `None` for `auto`, `decltype` and friends, where the rendered string is kept.
    */
  def forSimpleDeclSpecifier(spec: IASTSimpleDeclSpecifier): Option[String] = {
    val base = spec.getType match {
      case IASTSimpleDeclSpecifier.t_char     => Some("char")
      case IASTSimpleDeclSpecifier.t_wchar_t  => Some("wchar_t")
      case IASTSimpleDeclSpecifier.t_char8_t  => Some("char8_t")
      case IASTSimpleDeclSpecifier.t_char16_t => Some("char16_t")
      case IASTSimpleDeclSpecifier.t_char32_t => Some("char32_t")
      case IASTSimpleDeclSpecifier.t_bool     =>
        Some(if (spec.isInstanceOf[ICPPASTSimpleDeclSpecifier]) "bool" else "_Bool")
      case IASTSimpleDeclSpecifier.t_int128 => Some("__int128")
      case IASTSimpleDeclSpecifier.t_void   => Some("void")
      case IASTSimpleDeclSpecifier.t_float  => Some("float")
      case IASTSimpleDeclSpecifier.t_double => Some(if (spec.isLong) "long double" else "double")
      case IASTSimpleDeclSpecifier.t_int    =>
        Some(if (spec.isShort) "short" else if (spec.isLongLong) "long long" else if (spec.isLong) "long" else "int")
      case _ => None
    }
    base.map { name =>
      val withSign = withSignAndComplex(spec.isUnsigned, spec.isSigned, spec.isComplex, spec.isImaginary, name)
      if (spec.isVolatile) s"volatile $withSign" else withSign
    }
  }

  /** The name of `tpe` if it is a basic type, possibly below pointers and qualifiers: `const long *` is `long*` and
    * `volatile unsigned int` is `volatile unsigned int`. `None` for anything else (typedefs, references, arrays,
    * function types, named types), where CDT's rendered string is kept.
    */
  @nowarn("cat=deprecation")
  def renderBasic(tpe: IType): Option[String] = tpe match {
    case basic: ICPPBasicType if basic.getQualifierBits != 0 => None
    case basic: IBasicType                                   => forBasicType(basic)
    case ptr: IPointerType                                   => renderBasic(ptr.getType).map(_ + "*")
    case qual: IQualifierType                                =>
      renderBasic(qual.getType).map(inner => if (qual.isVolatile) s"volatile $inner" else inner)
    case _: ITypedef => None
    case _           => None
  }

  private def withSignAndComplex(
    isUnsigned: Boolean,
    isSigned: Boolean,
    isComplex: Boolean,
    isImaginary: Boolean,
    base: String
  ): String = {
    val withSign = if (isUnsigned) s"unsigned $base" else if (isSigned && base == "char") s"signed $base" else base
    if (isComplex) s"$withSign _Complex" else if (isImaginary) s"$withSign _Imaginary" else withSign
  }

}
