package io.shiftleft.semanticcpg.language.operatorextension

import io.shiftleft.codepropertygraph.generated.nodes.{Call, StaticType}

trait AssignmentT
trait ArithmeticT
trait ArrayAccessT
trait FieldAccessT
object OpNodes {
  type Assignment  = Call with StaticType[AssignmentT]
  type Arithmetic  = Call with StaticType[ArithmeticT]
  type ArrayAccess = Call with StaticType[ArrayAccessT]
  type FieldAccess = Call with StaticType[FieldAccessT]
}
