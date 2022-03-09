package io.joern.kotlin2cpg.types

object NameReferenceKinds extends Enumeration {
  type NameReferenceKind = Value
  val Unknown, ClassName, EnumEntry, LocalVariable, Property = Value
}
