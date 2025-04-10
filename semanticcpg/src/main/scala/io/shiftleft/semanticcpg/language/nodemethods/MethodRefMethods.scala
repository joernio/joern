package io.shiftleft.semanticcpg.language.nodemethods

import io.shiftleft.codepropertygraph.generated.nodes.{MethodRef, NewLocation}
import io.shiftleft.semanticcpg.NodeExtension
import io.shiftleft.semanticcpg.language.*

class MethodRefMethods(val methodRef: MethodRef) extends AnyVal with NodeExtension with HasLocation with HasLoc {
  @deprecated("Prefer .loc to .location")
  override def location: NewLocation = {
    LocationCreator(
      methodRef,
      methodRef.code,
      methodRef.label,
      methodRef.lineNumber,
      methodRef._methodViaContainsIn.next()
    )
  }

  override def loc: Loc = {
    Loc(methodRef)
  }
}
