package io.joern.scanners.c

import io.shiftleft.codepropertygraph.generated.{Operators, nodes}
import overflowdb.traversal.Traversal
import io.shiftleft.semanticcpg.language._
import io.shiftleft.semanticcpg.language.operatorextension.OpNodes

object QueryLangExtensions {

  implicit class CallExtension(callTrav: Traversal[nodes.Call]) {
    def returnValueNotChecked: Traversal[nodes.Call] = {
      val notDirectlyChecked = callTrav.filterNot { y =>
        val code = y.code
        y.inAstMinusLeaf.isControlStructure.condition.code.exists { x =>
          x.contains(code)
        }
      }
      val notInCondition = notDirectlyChecked.filterNot { call =>
        val inConditions = call.method.controlStructure.condition.ast.l;
        val checkedVars  = inConditions.isIdentifier.name.toSet ++ inConditions.isCall.code.toSet;
        val targets      = call.inAssignment.target.code.toSet
        (targets & checkedVars).nonEmpty
      }
      notInCondition.whereNot { call =>
        call.inAstMinusLeaf.isReturn
      }
    }
  }

  implicit class LiteralExtension(litTrav: Traversal[nodes.Literal]) {

    def toInt: Traversal[Int] = {
      litTrav.code.flatMap { lit =>
        try {
          List(lit.toInt)
        } catch {
          case _: Exception => List()
        }
      }
    }
  }

}
