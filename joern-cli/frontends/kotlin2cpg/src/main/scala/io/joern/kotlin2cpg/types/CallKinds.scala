package io.joern.kotlin2cpg.types

object CallKinds extends Enumeration {
  type CallKind = Value
  val Unknown, StaticCall, DynamicCall, ExtensionCall = Value
}

class CallKinds {}
