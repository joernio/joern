package io.joern.scanners.c

import io.shiftleft.semanticcpg.language.{ICallResolver, NoResolve}
import io.joern.scanners._
import io.joern.console._
import io.shiftleft.codepropertygraph.generated.nodes
import io.joern.dataflowengineoss.queryengine.EngineContext
import io.shiftleft.semanticcpg.language._
import io.joern.macros.QueryMacros._
import io.shiftleft.semanticcpg.language.operatorextension._
import QueryLangExtensions._

object MissingLengthCheck extends QueryBundle {

  implicit val resolver: ICallResolver = NoResolve

  @q
  def constantArrayAccessNoCheck()(implicit context: EngineContext): Query =
    Query.make(
      name = "constant-array-access-no-check",
      author = Crew.fabs,
      title = "Array access at fixed offset but sufficient length check not determined",
      description = """
          |
          |""".stripMargin,
      score = 3,
      withStrRep({ cpg =>
        cpg.method.arrayAccess
          .filter { access =>
            val arrName = access.simpleName
            !arrName.isEmpty && !arrName.forall(x => access.method.local.nameExact(x).nonEmpty)
          }
          .usesConstantOffset
          .flatMap { arrayAccess =>
            val lenFields =
              potentialLengthFields(arrayAccess, arrayAccess.method)
            if (lenFields.nonEmpty) {
              List((arrayAccess, lenFields))
            } else {
              List()
            }
          }
          .collect {
            case (arrayAccess, lenFields) if !checked(arrayAccess, lenFields) =>
              arrayAccess
          }
      }),
      tags = List(QueryTags.default)
    )

  /** Names of potential length fields for the array named `arrayName` in the method `method`. Determined heuristically
    * via name matching.
    */
  private def potentialLengthFields(arrayAccess: OpNodes.ArrayAccess, method: nodes.Method): List[String] = {
    val arrayName = arrayAccess.simpleName.head
    List(arrayName).flatMap { name =>
      val normalizedName = name.replaceAll("s$", "")
      val regex          = s"(?i)$normalizedName(s?)(_?)(len|siz).*"
      method.parameter.name(regex).name.l ++
        method.local.name(regex).name.l
    }
  }

  /** For a given array access with a single constant offset and a set of variable names of potential length fields of
    * the array, determine whether a check of at least one of the potential length fields exist for each literal
    */
  def checked(arrayAccess: OpNodes.ArrayAccess, lens: List[String]): Boolean = {
    val arrayIndex = arrayAccess.argument(2).ast.isLiteral.toInt.head
    val lowerBounds = arrayAccess.method.controlStructure.condition
      .where(_.ast.isIdentifier.name.filter { n =>
        lens.contains(n)
      })
      .ast
      .isLiteral
      .toInt ++ {
      if (
        arrayAccess.method.controlStructure.condition
          .codeExact(arrayAccess.array.code)
          .nonEmpty
      ) {
        List(0)
      } else {
        List()
      }
    }
    lowerBounds.exists { bound =>
      bound >= arrayIndex
    }
  }

}
