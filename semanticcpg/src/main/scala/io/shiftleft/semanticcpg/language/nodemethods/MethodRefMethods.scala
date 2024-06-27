package io.shiftleft.semanticcpg.language.nodemethods

import io.shiftleft.codepropertygraph.generated.nodes.{MethodRef, NewLocation}
import io.shiftleft.semanticcpg.NodeExtension
import io.shiftleft.semanticcpg.language.*

class MethodRefMethods(val methodRef: MethodRef) extends AnyVal with NodeExtension with HasLocation {
  override def location: NewLocation = {
    LocationCreator(
      methodRef,
      methodRef.code,
      methodRef.label,
      methodRef.lineNumber,
      methodRef._methodViaContainsIn.next()
    )
  }
}
