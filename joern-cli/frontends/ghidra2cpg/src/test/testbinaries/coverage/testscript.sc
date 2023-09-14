import io.shiftleft.codepropertygraph.generated.Cpg
import io.joern.dataflowengineoss.language._
import io.shiftleft.semanticcpg.language._
import io.shiftleft.semanticcpg.language.operatorextension.OpNodes.Assignment
import flatgraph.traversal._

@main def main(testBinary: String) = {
  importCode.ghidra(testBinary)
  val functions  = cpg.method.size
  val calls      = cpg.method.call.size
  val parameters = cpg.parameter.size
  val locals     = cpg.local.size
  println(s"""
functions ${functions}
calls ${calls}
parameters ${parameters}
locals ${locals}
  """)
}
