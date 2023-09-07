package io.joern.scanners.c

import io.shiftleft.codepropertygraph.generated.nodes
import io.shiftleft.semanticcpg.language.*

import scala.util.Try

object QueryLangExtensions {

  implicit class CallExtension(callTrav: Iterator[nodes.Call]) {
    def returnValueNotChecked: Iterator[nodes.Call] = {
      val notDirectlyChecked = callTrav.filterNot { y =>
        val code = y.code
        y.inAstMinusLeaf.isControlStructure.condition.code.exists { x =>
          x.contains(code)
        }
      }
      val notInCondition = notDirectlyChecked.filterNot { call =>
        val inConditions = call.method.controlStructure.condition.ast.l
        val checkedVars  = inConditions.isIdentifier.name.toSet ++ inConditions.isCall.code.toSet
        val targets      = call.inAssignment.target.code.toSet
        (targets & checkedVars).nonEmpty
      }
      notInCondition.whereNot { call =>
        call.inAstMinusLeaf.isReturn
      }
    }
  }

  implicit class LiteralExtension(litTrav: Iterator[nodes.Literal]) {

    def toInt: Iterator[Int] = {
      litTrav.code.flatMap { lit => Try(Integer.decode(lit).intValue()).toOption }
    }
  }

}
