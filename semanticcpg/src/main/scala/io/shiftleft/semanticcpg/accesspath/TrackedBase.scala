package io.shiftleft.semanticcpg.accesspath

import io.shiftleft.codepropertygraph.generated.nodes._

trait TrackedBase
case class TrackedNamedVariable(name: String) extends TrackedBase
case class TrackedReturnValue(call: CallRepr) extends TrackedBase {
  override def toString: String = {
    s"TrackedReturnValue(${call.code})"
  }
}
case class TrackedLiteral(literal: Literal) extends TrackedBase {
  override def toString: String = {
    s"TrackedLiteral(${literal.code})"
  }
}
case class TrackedMethodOrTypeRef(methodOrTypeRef: StoredNode with HasCode) extends TrackedBase {
  override def toString: String = {
    s"TrackedMethodOrTypeRef(${methodOrTypeRef.code})"
  }
}

case class TrackedAlias(argIndex: Int) extends TrackedBase {
  override def toString: String = {
    s"TrackedAlias($argIndex)"
  }
}

object TrackedUnknown extends TrackedBase {
  override def toString: String = {
    "TrackedUnknown"
  }
}
object TrackedFormalReturn extends TrackedBase {
  override def toString: String = {
    "TrackedFormalReturn"
  }
}
