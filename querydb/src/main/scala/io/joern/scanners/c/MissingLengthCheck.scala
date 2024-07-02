package io.joern.scanners.c

import io.shiftleft.semanticcpg.language.{ICallResolver, NoResolve}
import io.joern.scanners.*
import io.joern.console.*
import io.shiftleft.codepropertygraph.generated.nodes
import io.joern.dataflowengineoss.queryengine.EngineContext
import io.shiftleft.semanticcpg.language.*
import io.joern.macros.QueryMacros.*
import io.shiftleft.semanticcpg.language.operatorextension.*
import QueryLangExtensions.*

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
            arrName.nonEmpty && !arrName.forall(x => access.method.local.nameExact(x).nonEmpty)
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
  private def potentialLengthFields(arrayAccess: OpNodes.ArrayAccess, method: nodes.Method): Seq[String] = {
    arrayAccess.simpleName.flatMap { name =>
      val normalizedName = name.stripSuffix("s")
      val regex          = s"(?i)$normalizedName(s?)(_?)(len|siz).*"
      method.parameter.name(regex).name ++
        method.local.name(regex).name
    }.toSeq
  }

  /** For a given array access with a single constant offset and a set of variable names of potential length fields of
    * the array, determine whether a check of at least one of the potential length fields exist for each literal
    */
  def checked(arrayAccess: OpNodes.ArrayAccess, lens: Seq[String]): Boolean = {
    val arrayIndex = arrayAccess.argument(2).ast.isLiteral.toInt.headOption match {
      case Some(x) => x
      case None    => return true // we can only deal with literals as indices here
    }

    val lowerBounds = arrayAccess.method.controlStructure.condition
      .where(_.ast.isIdentifier.nameExact(lens*))
      .ast
      .isLiteral
      .toInt ++
      Option.when(
        arrayAccess.method.controlStructure.condition
          .codeExact(arrayAccess.array.code)
          .nonEmpty
      )(0)
    lowerBounds.exists { bound =>
      bound >= arrayIndex
    }
  }

}
