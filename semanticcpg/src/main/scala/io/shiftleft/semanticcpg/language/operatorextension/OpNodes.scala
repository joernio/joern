package io.shiftleft.semanticcpg.language.operatorextension

import io.shiftleft.codepropertygraph.generated.v2.nodes.Call

object OpNodes {
  class Assignment(call: Call)  extends Call(call.graph, call.seq())
  class Arithmetic(call: Call)  extends Call(call.graph, call.seq())
  class ArrayAccess(call: Call) extends Call(call.graph, call.seq())
  class FieldAccess(call: Call) extends Call(call.graph, call.seq())
}
