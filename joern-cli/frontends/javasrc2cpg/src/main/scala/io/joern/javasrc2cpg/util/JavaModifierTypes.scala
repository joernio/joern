package io.joern.javasrc2cpg.util

/** Used for Java-specific modifiers that aren't used by other languages
  */
object JavaModifierTypes {
  val Sealed: String    = "SEALED"
  val NonSealed: String = "NON_SEALED"
  val Strictfp: String  = "STRICTFP"
}
