package io.shiftleft.semanticcpg.language.operatorextension

import io.shiftleft.codepropertygraph.generated.nodes.Call

object OpNodes {
  class Assignment(call: Call)  extends Call(call.graph, call.id)
  class Arithmetic(call: Call)  extends Call(call.graph, call.id)
  class ArrayAccess(call: Call) extends Call(call.graph, call.id)
  class FieldAccess(call: Call) extends Call(call.graph, call.id)
}
