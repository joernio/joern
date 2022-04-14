package io.joern.c2cpg.astcreation

import io.shiftleft.codepropertygraph.generated.nodes.{NewBlock, NewCall, NewIdentifier, NewMethodRef}
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, Operators}
import io.joern.x2cpg.Ast
import org.eclipse.cdt.core.dom.ast._
import org.eclipse.cdt.core.dom.ast.cpp._
import org.eclipse.cdt.core.dom.ast.gnu.IGNUASTCompoundStatementExpression
import org.eclipse.cdt.internal.core.dom.parser.cpp.CPPASTQualifiedName

trait AstForExpressionsCreator {

  this: AstCreator =>

  private def astForBinaryExpression(bin: IASTBinaryExpression, argIndex: Int): Ast = {
    val op = bin.getOperator match {
      case IASTBinaryExpression.op_multiply         => Operators.multiplication
      case IASTBinaryExpression.op_divide           => Operators.division
      case IASTBinaryExpression.op_modulo           => Operators.modulo
      case IASTBinaryExpression.op_plus             => Operators.addition
      case IASTBinaryExpression.op_minus            => Operators.subtraction
      case IASTBinaryExpression.op_shiftLeft        => Operators.shiftLeft
      case IASTBinaryExpression.op_shiftRight       => Operators.arithmeticShiftRight
      case IASTBinaryExpression.op_lessThan         => Operators.lessThan
      case IASTBinaryExpression.op_greaterThan      => Operators.greaterThan
      case IASTBinaryExpression.op_lessEqual        => Operators.lessEqualsThan
      case IASTBinaryExpression.op_greaterEqual     => Operators.greaterEqualsThan
      case IASTBinaryExpression.op_binaryAnd        => Operators.and
      case IASTBinaryExpression.op_binaryXor        => Operators.xor
      case IASTBinaryExpression.op_binaryOr         => Operators.or
      case IASTBinaryExpression.op_logicalAnd       => Operators.logicalAnd
      case IASTBinaryExpression.op_logicalOr        => Operators.logicalOr
      case IASTBinaryExpression.op_assign           => Operators.assignment
      case IASTBinaryExpression.op_multiplyAssign   => Operators.assignmentMultiplication
      case IASTBinaryExpression.op_divideAssign     => Operators.assignmentDivision
      case IASTBinaryExpression.op_moduloAssign     => Operators.assignmentModulo
      case IASTBinaryExpression.op_plusAssign       => Operators.assignmentPlus
      case IASTBinaryExpression.op_minusAssign      => Operators.assignmentMinus
      case IASTBinaryExpression.op_shiftLeftAssign  => Operators.assignmentShiftLeft
      case IASTBinaryExpression.op_shiftRightAssign => Operators.assignmentArithmeticShiftRight
      case IASTBinaryExpression.op_binaryAndAssign  => Operators.assignmentAnd
      case IASTBinaryExpression.op_binaryXorAssign  => Operators.assignmentXor
      case IASTBinaryExpression.op_binaryOrAssign   => Operators.assignmentOr
      case IASTBinaryExpression.op_equals           => Operators.equals
      case IASTBinaryExpression.op_notequals        => Operators.notEquals
      case IASTBinaryExpression.op_pmdot            => Operators.indirectFieldAccess
      case IASTBinaryExpression.op_pmarrow          => Operators.indirectFieldAccess
      case IASTBinaryExpression.op_max              => "<operator>.max"
      case IASTBinaryExpression.op_min              => "<operator>.min"
      case IASTBinaryExpression.op_ellipses         => "<operator>.op_ellipses"
      case _                                        => "<operator>.unknown"
    }

    val callNode = newCallNode(bin, op, op, DispatchTypes.STATIC_DISPATCH, argIndex)
    val left     = nullSafeAst(bin.getOperand1)
    val right    = nullSafeAst(bin.getOperand2)
    callAst(callNode, List(left, right))
  }

  private def astForExpressionList(exprList: IASTExpressionList, argIndex: Int): Ast = {
    val b = NewBlock()
      .argumentIndex(argIndex)
      .typeFullName(registerType(Defines.voidTypeName))
      .lineNumber(line(exprList))
      .columnNumber(column(exprList))
    Ast(b).withChildren(exprList.getExpressions.toIndexedSeq.map(astForExpression(_, argIndex)))
  }

  private def astForCallExpression(call: IASTFunctionCallExpression, argIndex: Int): Ast = {
    val rec = call.getFunctionNameExpression match {
      case unaryExpression: IASTUnaryExpression if unaryExpression.getOperand.isInstanceOf[IASTBinaryExpression] =>
        astForBinaryExpression(unaryExpression.getOperand.asInstanceOf[IASTBinaryExpression], 0)
      case unaryExpression: IASTUnaryExpression if unaryExpression.getOperand.isInstanceOf[IASTFieldReference] =>
        astForFieldReference(unaryExpression.getOperand.asInstanceOf[IASTFieldReference], 0)
      case unaryExpression: IASTUnaryExpression
          if unaryExpression.getOperand.isInstanceOf[IASTArraySubscriptExpression] =>
        astForArrayIndexExpression(unaryExpression.getOperand.asInstanceOf[IASTArraySubscriptExpression], 0)
      case unaryExpression: IASTUnaryExpression if unaryExpression.getOperand.isInstanceOf[IASTConditionalExpression] =>
        astForUnaryExpression(unaryExpression, 0)
      case unaryExpression: IASTUnaryExpression if unaryExpression.getOperand.isInstanceOf[IASTUnaryExpression] =>
        astForUnaryExpression(unaryExpression.getOperand.asInstanceOf[IASTUnaryExpression], 0)
      case lambdaExpression: ICPPASTLambdaExpression =>
        val methodRefAst = astForMethodRefForLambda(lambdaExpression)
        methodRefAst.root.get.asInstanceOf[NewMethodRef].argumentIndex = 0
        methodRefAst
      case other => astForExpression(other, 0)
    }

    val (dd, name) = call.getFunctionNameExpression match {
      case _: ICPPASTLambdaExpression =>
        (DispatchTypes.STATIC_DISPATCH, rec.root.get.asInstanceOf[NewMethodRef].methodFullName)
      case _ if rec.root.exists(_.isInstanceOf[NewIdentifier]) =>
        (DispatchTypes.STATIC_DISPATCH, rec.root.get.asInstanceOf[NewIdentifier].name)
      case _ if rec.root.exists(_.isInstanceOf[NewCall]) =>
        (DispatchTypes.STATIC_DISPATCH, rec.root.get.asInstanceOf[NewCall].code)
      case reference: IASTIdExpression =>
        (DispatchTypes.STATIC_DISPATCH, nodeSignature(reference))
      case _ =>
        (DispatchTypes.STATIC_DISPATCH, "")
    }

    val cpgCall = newCallNode(call, name, name, dd, argIndex)
    val args    = call.getArguments.toList.map(a => astForNode(a))
    rec.root match {
      // Optimization: do not include the receiver if the receiver is just the function name,
      // e.g., for `f(x)`, don't include an `f` identifier node as a first child. Since we
      // have so many call sites in CPGs, this drastically reduces the number of nodes.
      // Moreover, the data flow tracker does not need to track `f`, which would not make
      // much sense anyway.
      case Some(r: NewIdentifier) if r.name == name =>
        callAst(cpgCall, args)
      case Some(_) =>
        callAst(cpgCall, args, Some(rec))
      case None =>
        callAst(cpgCall, args)
    }
  }

  private def astForUnaryExpression(unary: IASTUnaryExpression, argIndex: Int): Ast = {
    val operatorMethod = unary.getOperator match {
      case IASTUnaryExpression.op_prefixIncr  => Operators.preIncrement
      case IASTUnaryExpression.op_prefixDecr  => Operators.preDecrement
      case IASTUnaryExpression.op_plus        => Operators.plus
      case IASTUnaryExpression.op_minus       => Operators.minus
      case IASTUnaryExpression.op_star        => Operators.indirection
      case IASTUnaryExpression.op_amper       => Operators.addressOf
      case IASTUnaryExpression.op_tilde       => Operators.not
      case IASTUnaryExpression.op_not         => Operators.logicalNot
      case IASTUnaryExpression.op_sizeof      => Operators.sizeOf
      case IASTUnaryExpression.op_postFixIncr => Operators.postIncrement
      case IASTUnaryExpression.op_postFixDecr => Operators.postDecrement
      case IASTUnaryExpression.op_throw       => "<operator>.throw"
      case IASTUnaryExpression.op_typeid      => "<operator>.typeOf"
      case _                                  => "<operator>.unknown"
    }

    if (unary.getOperator == IASTUnaryExpression.op_bracketedPrimary) {
      astForExpression(unary.getOperand, argIndex)
    } else {
      val cpgUnary = newCallNode(unary, operatorMethod, operatorMethod, DispatchTypes.STATIC_DISPATCH, argIndex)
      val operandExpr = unary.getOperand match {
        // special handling for operand expression in brackets - we simply ignore the brackets
        case opExpr: IASTUnaryExpression if opExpr.getOperator == IASTUnaryExpression.op_bracketedPrimary =>
          opExpr.getOperand
        case opExpr => opExpr
      }
      val operand = nullSafeAst(operandExpr)
      callAst(cpgUnary, List(operand))
    }
  }

  private def astForTypeIdExpression(typeId: IASTTypeIdExpression, argIndex: Int): Ast = {
    typeId.getOperator match {
      case op
          if op == IASTTypeIdExpression.op_sizeof ||
            op == IASTTypeIdExpression.op_sizeofParameterPack ||
            op == IASTTypeIdExpression.op_typeid ||
            op == IASTTypeIdExpression.op_alignof ||
            op == IASTTypeIdExpression.op_typeof =>
        val call = newCallNode(typeId, Operators.sizeOf, Operators.sizeOf, DispatchTypes.STATIC_DISPATCH, argIndex)
        val arg  = astForNode(typeId.getTypeId.getDeclSpecifier)
        callAst(call, List(arg))
      case _ => notHandledYet(typeId, argIndex)
    }
  }

  private def astForConditionalExpression(expr: IASTConditionalExpression, argIndex: Int): Ast = {
    val call = newCallNode(expr, Operators.conditional, Operators.conditional, DispatchTypes.STATIC_DISPATCH, argIndex)

    val condAst = nullSafeAst(expr.getLogicalConditionExpression)
    val posAst  = nullSafeAst(expr.getPositiveResultExpression)
    val negAst  = nullSafeAst(expr.getNegativeResultExpression)

    val children = List(condAst, posAst, negAst)
    callAst(call, children)
  }

  private def astForArrayIndexExpression(arrayIndexExpression: IASTArraySubscriptExpression, argIndex: Int): Ast = {
    val cpgArrayIndexing =
      newCallNode(
        arrayIndexExpression,
        Operators.indirectIndexAccess,
        Operators.indirectIndexAccess,
        DispatchTypes.STATIC_DISPATCH,
        argIndex
      )

    val expr = astForExpression(arrayIndexExpression.getArrayExpression, 1)
    val arg  = astForNode(arrayIndexExpression.getArgument, 2)
    callAst(cpgArrayIndexing, List(expr, arg))
  }

  private def astForCastExpression(castExpression: IASTCastExpression, argIndex: Int): Ast = {
    val cpgCastExpression =
      newCallNode(castExpression, Operators.cast, Operators.cast, DispatchTypes.STATIC_DISPATCH, argIndex)

    val expr    = astForExpression(castExpression.getOperand, 2)
    val argNode = castExpression.getTypeId
    val arg     = newUnknown(argNode, 1)

    callAst(cpgCastExpression, List(Ast(arg), expr))
  }

  private def astForNewExpression(newExpression: ICPPASTNewExpression, argIndex: Int): Ast = {
    val cpgNewExpression =
      newCallNode(newExpression, "<operator>.new", "<operator>.new", DispatchTypes.STATIC_DISPATCH, argIndex)

    val typeId = newExpression.getTypeId
    if (newExpression.isArrayAllocation) {
      val cpgTypeId = astForIdentifier(typeId.getDeclSpecifier, 1)
      Ast(cpgNewExpression).withChild(cpgTypeId).withArgEdge(cpgNewExpression, cpgTypeId.root.get)
    } else {
      val cpgTypeId = astForIdentifier(typeId.getDeclSpecifier, 1)
      val args =
        if (
          newExpression.getInitializer != null && newExpression.getInitializer
            .isInstanceOf[ICPPASTConstructorInitializer]
        ) {
          val args = newExpression.getInitializer.asInstanceOf[ICPPASTConstructorInitializer].getArguments
          withIndex(args) { (a, i) =>
            astForNode(a, 1 + i)
          }
        } else {
          Seq.empty
        }

      callAst(cpgNewExpression, List(cpgTypeId) ++ args)
    }
  }

  private def astForDeleteExpression(delExpression: ICPPASTDeleteExpression, argIndex: Int): Ast = {
    val cpgDeleteNode =
      newCallNode(delExpression, Operators.delete, Operators.delete, DispatchTypes.STATIC_DISPATCH, argIndex)
    val arg = astForExpression(delExpression.getOperand, 1)
    callAst(cpgDeleteNode, List(arg))
  }

  private def astForTypeIdInitExpression(typeIdInit: IASTTypeIdInitializerExpression, argIndex: Int): Ast = {
    val cpgCastExpression =
      newCallNode(typeIdInit, Operators.cast, Operators.cast, DispatchTypes.STATIC_DISPATCH, argIndex)

    val typeAst = newUnknown(typeIdInit.getTypeId)
    val expr    = astForNode(typeIdInit.getInitializer)
    callAst(cpgCastExpression, List(Ast(typeAst), expr))
  }

  private def astForConstructorExpression(c: ICPPASTSimpleTypeConstructorExpression): Ast = {
    val name     = c.getDeclSpecifier.toString
    val callNode = newCallNode(c, name, name, DispatchTypes.STATIC_DISPATCH)
    val arg      = astForNode(c.getInitializer)
    callAst(callNode, List(arg))
  }

  private def astForCompoundStatementExpression(
    compoundExpression: IGNUASTCompoundStatementExpression,
    argIndex: Int
  ): Ast =
    nullSafeAst(compoundExpression.getCompoundStatement, argIndex).headOption.getOrElse(Ast())

  private def astForPackExpansionExpression(
    packExpansionExpression: ICPPASTPackExpansionExpression,
    argIndex: Int
  ): Ast =
    astForExpression(packExpansionExpression.getPattern, argIndex)

  protected def astForExpression(expression: IASTExpression, argIndex: Int = -1): Ast = {
    val r = expression match {
      case lit: IASTLiteralExpression   => astForLiteral(lit, argIndex)
      case un: IASTUnaryExpression      => astForUnaryExpression(un, argIndex)
      case bin: IASTBinaryExpression    => astForBinaryExpression(bin, argIndex)
      case exprList: IASTExpressionList => astForExpressionList(exprList, argIndex)
      case qualId: IASTIdExpression if qualId.getName.isInstanceOf[CPPASTQualifiedName] =>
        astForQualifiedName(qualId.getName.asInstanceOf[CPPASTQualifiedName], argIndex)
      case ident: IASTIdExpression          => astForIdentifier(ident, argIndex)
      case call: IASTFunctionCallExpression => astForCallExpression(call, argIndex)
      case typeId: IASTTypeIdExpression     => astForTypeIdExpression(typeId, argIndex)
      case fieldRef: IASTFieldReference     => astForFieldReference(fieldRef, argIndex)
      case expr: IASTConditionalExpression  => astForConditionalExpression(expr, argIndex)
      case arrayIndexExpression: IASTArraySubscriptExpression =>
        astForArrayIndexExpression(arrayIndexExpression, argIndex)
      case castExpression: IASTCastExpression          => astForCastExpression(castExpression, argIndex)
      case newExpression: ICPPASTNewExpression         => astForNewExpression(newExpression, argIndex)
      case delExpression: ICPPASTDeleteExpression      => astForDeleteExpression(delExpression, argIndex)
      case typeIdInit: IASTTypeIdInitializerExpression => astForTypeIdInitExpression(typeIdInit, argIndex)
      case c: ICPPASTSimpleTypeConstructorExpression   => astForConstructorExpression(c)
      case lambdaExpression: ICPPASTLambdaExpression   => astForMethodRefForLambda(lambdaExpression)
      case compoundExpression: IGNUASTCompoundStatementExpression =>
        astForCompoundStatementExpression(compoundExpression, argIndex)
      case packExpansionExpression: ICPPASTPackExpansionExpression =>
        astForPackExpansionExpression(packExpansionExpression, argIndex)
      case _ => notHandledYet(expression, argIndex)
    }
    asChildOfMacroCall(expression, r, argIndex)
  }

  protected def astForStaticAssert(a: ICPPASTStaticAssertDeclaration): Ast = {
    val name  = "static_assert"
    val call  = newCallNode(a, name, name, DispatchTypes.STATIC_DISPATCH)
    val cond  = nullSafeAst(a.getCondition)
    val messg = nullSafeAst(a.getMessage)
    callAst(call, List(cond, messg))
  }

}
