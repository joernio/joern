package io.joern.scanners.c

import io.shiftleft.codepropertygraph.generated.{Operators, nodes}
import overflowdb.traversal.Traversal
import io.shiftleft.semanticcpg.language._
import io.shiftleft.semanticcpg.language.operatorextension.opnodes

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

    /**
      * For a given method, determine all array accesses at constant numeric offsets, e.g.,
      * `buf[10]` but not `buf[i + 10]`, and for simplicity, not even `buf[1+2]`,
      * `buf[PROBABLY_A_CONSTANT]` or `buf[PROBABLY_A_CONSTANT + 1]`.
      *
      * or even `buf[PROBABLY_A_CONSTANT]`.
      * */
    def arrayAccess: Traversal[opnodes.ArrayAccess] = {
      methodTrav.call
        .nameExact(Operators.indirectIndexAccess)
        .map(new opnodes.ArrayAccess(_))
    }

  }

  implicit class ArrayAccessExtension(
      arrayAccessTrav: Traversal[opnodes.ArrayAccess]) {

    def usesConstantOffset: Traversal[opnodes.ArrayAccess] = {
      arrayAccessTrav
        .whereNot(_.argument(2).ast.isIdentifier)
        .filter { x =>
          val literalIndices = x.argument(2).ast.isLiteral.l
          literalIndices.size == 1
        }
    }

  }

  implicit class ArrayAccessNodeExtension(arrayAccess: opnodes.ArrayAccess) {

    /**
      * If the expression on the left side of the array access is a lone
      * identifier, return it.
      * */
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
