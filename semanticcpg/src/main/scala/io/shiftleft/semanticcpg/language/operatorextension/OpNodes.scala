package io.shiftleft.semanticcpg.language.operatorextension

import io.shiftleft.codepropertygraph.generated.nodes.{Call, StaticType}

trait AssignmentT
trait ArithmeticT
trait ArrayAccessT
trait FieldAccessT
object OpNodes {
  type Assignment  = Call & StaticType[AssignmentT]
  type Arithmetic  = Call & StaticType[ArithmeticT]
  type ArrayAccess = Call & StaticType[ArrayAccessT]
  type FieldAccess = Call & StaticType[FieldAccessT]
}
