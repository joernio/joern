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
        val checkedVars = inConditions.isIdentifier.name.toSet ++ inConditions.isCall.code.toSet;
        val targets = call.inAssignment.target.code.toSet
        (targets & checkedVars).nonEmpty
      }
      notInCondition.whereNot { call =>
        call.inAstMinusLeaf.isReturn
      }
    }
  }

  implicit class MethodExtension(methodTrav: Traversal[nodes.Method]) {
    def arrayAccess: Traversal[OpNodes.ArrayAccess] = {
      methodTrav.call
        .nameExact(Operators.indirectIndexAccess)
        .map(new OpNodes.ArrayAccess(_))
    }
  }

  implicit class ArrayAccessNodeExtension(arrayAccess: OpNodes.ArrayAccess) {

    /** If the expression on the left side of the array access is a lone
      * identifier, return it.
      */
    def simpleName: Option[String] = {
      // TODO should be part of the standard language
      // TODO language is a bit clumsy here. Should be something like
      // `arrayAccess.array.identifier.name.headOption`
      arrayAccess.array
        .where(_.isIdentifier)
        .map(_.asInstanceOf[nodes.Identifier])
        .name
        .headOption
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
