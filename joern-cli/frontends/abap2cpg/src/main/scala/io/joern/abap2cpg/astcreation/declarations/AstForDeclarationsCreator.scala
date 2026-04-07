package io.joern.abap2cpg.astcreation.declarations

import io.joern.abap2cpg.astcreation.statements.AstForStatementsCreator
import io.joern.abap2cpg.parser.AbapIntermediateAst.*
import io.joern.abap2cpg.passes.AstCreator
import io.joern.x2cpg.Ast
import io.joern.x2cpg.datastructures.VariableScopeManager
import io.shiftleft.codepropertygraph.generated.EvaluationStrategies
import io.shiftleft.codepropertygraph.generated.nodes.*

/** Methods for creating method and type declaration nodes */
trait AstForDeclarationsCreator {
  this: AstCreator & AstForParametersCreator & AstForStatementsCreator =>

  /** Create a method AST */
  protected def astForMethod(method: MethodDef, className: Option[String], order: Int): Ast = {
    val fullName = className match {
      case Some(cls) => s"$cls::${method.name}"
      case None      => method.name
    }

    val signature = createSignature(method.parameters)

    val methodNode = NewMethod()
      .name(method.name)
      .fullName(fullName)
      .signature(signature)
      .filename(program.fileName)
      .isExternal(false)
      .order(order)

    method.span.start.foreach { pos =>
      methodNode.lineNumber(pos.row).columnNumber(pos.col)
    }

    // Start method scope
    // order is NOT set here — setOrderWhereNotSet will assign it after parameters (correct position)
    val blockNode = NewBlock()
      .typeFullName("void")
    scope.pushNewMethodScope(fullName, method.name, blockNode, None)

    // Add parameters
    var paramOrder    = 1
    val parameterAsts = scala.collection.mutable.ArrayBuffer[Ast]()

    // IMPORTING parameters
    method.parameters.importing.foreach { param =>
      val paramNode = createParameterIn(param, paramOrder)
      scope.addVariable(param.name, paramNode, param.typeName, VariableScopeManager.ScopeType.MethodScope)
      parameterAsts += Ast(paramNode)
      paramOrder += 1
    }

    // EXPORTING parameters
    method.parameters.exporting.foreach { param =>
      val paramNode = createParameterOut(param, paramOrder)
      parameterAsts += Ast(paramNode)
      paramOrder += 1
    }

    // CHANGING parameters (both IN and OUT)
    method.parameters.changing.foreach { param =>
      val paramInNode = createParameterIn(param, paramOrder)
      scope.addVariable(param.name, paramInNode, param.typeName, VariableScopeManager.ScopeType.MethodScope)
      parameterAsts += Ast(paramInNode)

      val paramOutNode = createParameterOut(param, paramOrder)
      parameterAsts += Ast(paramOutNode)
      paramOrder += 1
    }

    // RETURNING is a named output variable — create a LOCAL for it so body references
    // can find it via REF edges (METHOD_RETURN itself is unnamed per CPG spec).
    val (methodReturn, returningLocalAst) = method.parameters.returning match {
      case Some(returnParam) =>
        val localNode = NewLocal()
          .name(returnParam.name)
          .code(returnParam.name)
          .typeFullName(returnParam.typeName)
        scope.addVariable(returnParam.name, localNode, returnParam.typeName, VariableScopeManager.ScopeType.MethodScope)
        (createMethodReturn(returnParam), Some(Ast(localNode)))
      case None =>
        (
          NewMethodReturn()
            .evaluationStrategy(EvaluationStrategies.BY_REFERENCE)
            .typeFullName("void")
            .code("void"),
          None
        )
    }

    // Create body block with statements
    val stmtAsts = method.body match {
      case Some(stmtList) => astForStatements(stmtList, className)
      case None           => Seq.empty
    }

    // Add RETURNING local to block if it exists
    val allBlockChildren = returningLocalAst.toSeq ++ stmtAsts
    val blockAst         = Ast(blockNode).withChildren(allBlockChildren)

    scope.popScope()

    methodAst(methodNode, parameterAsts.toSeq, blockAst, methodReturn)
  }

  /** Create method signature string */
  protected def createSignature(params: MethodParameters): String = {
    val importingTypes = params.importing.map(_.typeName).mkString(", ")
    val exportingTypes = params.exporting.map(_.typeName).mkString(", ")
    val changingTypes  = params.changing.map(_.typeName).mkString(", ")
    val returnType     = params.returning.map(_.typeName).getOrElse("void")

    val allParams = Seq(importingTypes, exportingTypes, changingTypes)
      .filter(_.nonEmpty)
      .mkString("; ")

    if (allParams.nonEmpty) {
      s"($allParams) -> $returnType"
    } else {
      s"() -> $returnType"
    }
  }
}
