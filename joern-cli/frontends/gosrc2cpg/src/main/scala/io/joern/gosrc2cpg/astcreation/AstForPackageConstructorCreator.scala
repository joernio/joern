package io.joern.gosrc2cpg.astcreation

import io.joern.x2cpg.{Ast, AstCreatorBase, ValidationMode, Defines as XDefines}
import io.shiftleft.codepropertygraph.generated.{EvaluationStrategies, NodeTypes}
import io.shiftleft.codepropertygraph.generated.nodes.{NewBlock, NewMethod, NewMethodReturn}
import org.apache.commons.lang.StringUtils
import overflowdb.BatchedUpdate.DiffGraphBuilder

import scala.collection.immutable.Set

class AstForPackageConstructorCreator(val pacakgePath: String, statements: Set[(Ast, String)])(implicit
  withSchemaValidation: ValidationMode
) extends AstCreatorBase(pacakgePath) {

  override def createAst(): DiffGraphBuilder = {
    val name = StringUtils.normalizeSpace(s"$pacakgePath${XDefines.StaticInitMethodName}")
    val fakeGlobalMethod =
      NewMethod()
        .name(name)
        .code(name)
        .fullName(name)
        .filename(pacakgePath)
        .astParentType(NodeTypes.TYPE_DECL)
        .astParentFullName(pacakgePath)
        .isExternal(false)
        .lineNumber(0)
        .columnNumber(0)
        .lineNumberEnd(0)
        .columnNumberEnd(0)

    val blockNode_ = NewBlock()
      .code(name)
      .typeFullName(Defines.voidTypeName)
      .lineNumber(0)
      .columnNumber(0)

    val declsAsts = statements.map(_._1).toList
    setArgumentIndices(declsAsts)

    val methodReturn = NewMethodReturn()
      .typeFullName(Defines.voidTypeName)
      .code("RET")
      .evaluationStrategy(EvaluationStrategies.BY_VALUE)
      .lineNumber(0)
      .columnNumber(0)
    val ctorAst = methodAst(fakeGlobalMethod, Seq.empty, blockAst(blockNode_, declsAsts), methodReturn)
    Ast.storeInDiffGraph(ctorAst, diffGraph)
    diffGraph
  }

}
