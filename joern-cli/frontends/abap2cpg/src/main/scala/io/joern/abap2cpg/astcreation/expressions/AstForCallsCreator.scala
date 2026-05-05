package io.joern.abap2cpg.astcreation.expressions

import io.joern.abap2cpg.astcreation.AstHelpers
import io.joern.abap2cpg.parser.AbapIntermediateAst.*
import io.joern.abap2cpg.passes.AstCreator
import io.joern.x2cpg.Ast
import io.shiftleft.codepropertygraph.generated.{EvaluationStrategies, Operators}
import io.shiftleft.codepropertygraph.generated.nodes.*

/** Methods for creating call expression nodes (method calls, operators, field access) */
trait AstForCallsCreator { this: AstCreator & AstHelpers =>

  /** Create a call expression */
  protected def astForCall(callExpr: CallExpr, order: Int, className: Option[String]): Ast = {
    val methodFullName = (callExpr.targetName.isEmpty, callExpr.methodName) match {
      case (true, Some(method)) if method.startsWith("<operator>") =>
        method // operators are never class-qualified
      case (true, Some(method)) =>
        className match {
          case Some(cls) => s"$cls::$method"
          case None      => method
        }
      case (false, Some(method)) => s"${callExpr.targetName}.${method}"
      case (_, None)             => callExpr.targetName
    }

    val code = (callExpr.targetName.isEmpty, callExpr.methodName) match {
      case (true, Some(method)) => s"${method}()"
      case (false, Some(method)) =>
        val arrow = if (callExpr.isStatic) "=>" else "->"
        s"${callExpr.targetName}${arrow}${method}()"
      case (_, None) => s"${callExpr.targetName}()"
    }

    val callNode = NewCall()
      .name(callExpr.methodName.getOrElse(callExpr.targetName))
      .code(code)
      .methodFullName(methodFullName)
      .typeFullName("ANY")
      .dispatchType(if (callExpr.isStatic) "STATIC_DISPATCH" else "DYNAMIC_DISPATCH")
      .order(order)

    callExpr.span.start.foreach { pos =>
      callNode.lineNumber(pos.row).columnNumber(pos.col)
    }

    // Build receiver/base node for calls that have a target object or class.
    // The base is placed at argumentIndex=0 and wired via both ARGUMENT and RECEIVER
    // edges by callAst, making the receiver visible to the dataflow engine.
    // We skip placeholder targets ("<chained-result>") that have no source identity.
    val baseAst: Option[Ast] =
      if (callExpr.targetName.isEmpty || callExpr.targetName == "<chained-result>") {
        None
      } else {
        val receiverNode = NewIdentifier()
          .name(callExpr.targetName)
          .code(callExpr.targetName)
          .typeFullName("ANY")
        callExpr.span.start.foreach { pos =>
          receiverNode.lineNumber(pos.row).columnNumber(pos.col)
        }
        scope.addVariableReference(callExpr.targetName, receiverNode, "ANY", EvaluationStrategies.BY_REFERENCE)
        Some(Ast(receiverNode))
      }

    val argAsts = callExpr.arguments.zipWithIndex.map { case (arg, idx) =>
      val argAst = astForExpression(arg.value, idx + 1, className)

      // Set argumentName if provided (ABAP uses named parameters heavily)
      arg.name.foreach { paramName =>
        argAst.root.foreach {
          case expr: NewIdentifier => expr.argumentName(paramName)
          case expr: NewLiteral    => expr.argumentName(paramName)
          case expr: NewCall       => expr.argumentName(paramName)
          case _                   => ()
        }
      }

      argAst
    }

    callAst(callNode, argAsts, base = baseAst)
  }

  /** Create operator call */
  protected def astForOperatorCall(opCall: OperatorCall, order: Int, className: Option[String]): Ast = {
    val callNode = NewCall()
      .name(opCall.operatorName)
      .code(operatorCode(opCall))
      .methodFullName(opCall.operatorName)
      .typeFullName("ANY")
      .dispatchType("STATIC_DISPATCH")
      .order(order)

    opCall.span.start.foreach { pos =>
      callNode.lineNumber(pos.row).columnNumber(pos.col)
    }

    // fieldAccess and indirectFieldAccess require a FIELD_IDENTIFIER as the second argument.
    val argAsts = opCall.operatorName match {
      case Operators.fieldAccess | Operators.indirectFieldAccess if opCall.arguments.size == 2 =>
        val targetAst = astForExpression(opCall.arguments(0), 1, className)
        val fieldName = opCall.arguments(1) match {
          case IdentifierExpr(name, _) => name
          case other                   => codeFromExpr(other)
        }
        val fieldIdent = NewFieldIdentifier()
          .canonicalName(fieldName)
          .code(fieldName)
          .argumentIndex(2)
          .order(2)
        Seq(targetAst, Ast(fieldIdent))
      case _ =>
        opCall.arguments.zipWithIndex.map { case (arg, idx) =>
          astForExpression(arg, idx + 1, className)
        }
    }

    callAst(callNode, argAsts)
  }

  /** Create field access call */
  protected def astForFieldAccess(fieldAccess: FieldAccessExpr, order: Int): Ast = {
    val callNode = NewCall()
      .name(Operators.fieldAccess)
      .code(s"${codeFromExpr(fieldAccess.target)}-${fieldAccess.fieldName}")
      .methodFullName(Operators.fieldAccess)
      .typeFullName("ANY")
      .dispatchType("STATIC_DISPATCH")
      .order(order)

    fieldAccess.span.start.foreach { pos =>
      callNode.lineNumber(pos.row).columnNumber(pos.col)
    }

    val targetAst = astForExpression(fieldAccess.target, 1, None)

    val fieldIdent = NewFieldIdentifier()
      .canonicalName(fieldAccess.fieldName)
      .code(fieldAccess.fieldName)
      .argumentIndex(2)
      .order(2)

    callAst(callNode, Seq(targetAst, Ast(fieldIdent)))
  }
}
