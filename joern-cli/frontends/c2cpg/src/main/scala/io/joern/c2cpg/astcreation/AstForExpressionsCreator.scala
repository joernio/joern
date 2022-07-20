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

  private def astForBinaryExpression(bin: IASTBinaryExpression): Ast = {
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

    val callNode = newCallNode(bin, op, op, DispatchTypes.STATIC_DISPATCH)
    val left     = nullSafeAst(bin.getOperand1)
    val right    = nullSafeAst(bin.getOperand2)
    callAst(callNode, List(left, right))
  }

  private def astForExpressionList(exprList: IASTExpressionList): Ast = {
    val b = NewBlock()
      .typeFullName(registerType(Defines.voidTypeName))
      .lineNumber(line(exprList))
      .columnNumber(column(exprList))
    Ast(b).withChildren(exprList.getExpressions.toIndexedSeq.map(astForExpression))
  }

  private def astForCallExpression(call: IASTFunctionCallExpression): Ast = {
    val rec = call.getFunctionNameExpression match {
      case unaryExpression: IASTUnaryExpression if unaryExpression.getOperand.isInstanceOf[IASTBinaryExpression] =>
        astForBinaryExpression(unaryExpression.getOperand.asInstanceOf[IASTBinaryExpression])
      case unaryExpression: IASTUnaryExpression if unaryExpression.getOperand.isInstanceOf[IASTFieldReference] =>
        astForFieldReference(unaryExpression.getOperand.asInstanceOf[IASTFieldReference])
      case unaryExpression: IASTUnaryExpression
          if unaryExpression.getOperand.isInstanceOf[IASTArraySubscriptExpression] =>
        astForArrayIndexExpression(unaryExpression.getOperand.asInstanceOf[IASTArraySubscriptExpression])
      case unaryExpression: IASTUnaryExpression if unaryExpression.getOperand.isInstanceOf[IASTConditionalExpression] =>
        astForUnaryExpression(unaryExpression)
      case unaryExpression: IASTUnaryExpression if unaryExpression.getOperand.isInstanceOf[IASTUnaryExpression] =>
        astForUnaryExpression(unaryExpression.getOperand.asInstanceOf[IASTUnaryExpression])
      case lambdaExpression: ICPPASTLambdaExpression =>
        astForMethodRefForLambda(lambdaExpression)
      case other => astForExpression(other)
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

    val cpgCall = newCallNode(call, name, name, dd)
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

  private def astForUnaryExpression(unary: IASTUnaryExpression): Ast = {
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
      astForExpression(unary.getOperand)
    } else {
      val cpgUnary = newCallNode(unary, operatorMethod, operatorMethod, DispatchTypes.STATIC_DISPATCH)
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

  private def astForTypeIdExpression(typeId: IASTTypeIdExpression): Ast = {
    typeId.getOperator match {
      case op
          if op == IASTTypeIdExpression.op_sizeof ||
            op == IASTTypeIdExpression.op_sizeofParameterPack ||
            op == IASTTypeIdExpression.op_typeid ||
            op == IASTTypeIdExpression.op_alignof ||
            op == IASTTypeIdExpression.op_typeof =>
        val call = newCallNode(typeId, Operators.sizeOf, Operators.sizeOf, DispatchTypes.STATIC_DISPATCH)
        val arg  = astForNode(typeId.getTypeId.getDeclSpecifier)
        callAst(call, List(arg))
      case _ => notHandledYet(typeId)
    }
  }

  private def astForConditionalExpression(expr: IASTConditionalExpression): Ast = {
    val call = newCallNode(expr, Operators.conditional, Operators.conditional, DispatchTypes.STATIC_DISPATCH)

    val condAst = nullSafeAst(expr.getLogicalConditionExpression)
    val posAst  = nullSafeAst(expr.getPositiveResultExpression)
    val negAst  = nullSafeAst(expr.getNegativeResultExpression)

    val children = List(condAst, posAst, negAst)
    callAst(call, children)
  }

  private def astForArrayIndexExpression(arrayIndexExpression: IASTArraySubscriptExpression): Ast = {
    val cpgArrayIndexing =
      newCallNode(
        arrayIndexExpression,
        Operators.indirectIndexAccess,
        Operators.indirectIndexAccess,
        DispatchTypes.STATIC_DISPATCH
      )

    val expr = astForExpression(arrayIndexExpression.getArrayExpression)
    val arg  = astForNode(arrayIndexExpression.getArgument)
    callAst(cpgArrayIndexing, List(expr, arg))
  }

  private def astForCastExpression(castExpression: IASTCastExpression): Ast = {
    val cpgCastExpression =
      newCallNode(castExpression, Operators.cast, Operators.cast, DispatchTypes.STATIC_DISPATCH)

    val expr    = astForExpression(castExpression.getOperand)
    val argNode = castExpression.getTypeId
    val arg     = newUnknown(argNode)

    callAst(cpgCastExpression, List(Ast(arg), expr))
  }

  private def astForNewExpression(newExpression: ICPPASTNewExpression): Ast = {
    val cpgNewExpression =
      newCallNode(newExpression, "<operator>.new", "<operator>.new", DispatchTypes.STATIC_DISPATCH)

    val typeId = newExpression.getTypeId
    if (newExpression.isArrayAllocation) {
      val cpgTypeId = astForIdentifier(typeId.getDeclSpecifier)
      Ast(cpgNewExpression).withChild(cpgTypeId).withArgEdge(cpgNewExpression, cpgTypeId.root.get)
    } else {
      val cpgTypeId = astForIdentifier(typeId.getDeclSpecifier)
      val args =
        if (
          newExpression.getInitializer != null && newExpression.getInitializer
            .isInstanceOf[ICPPASTConstructorInitializer]
        ) {
          val args = newExpression.getInitializer.asInstanceOf[ICPPASTConstructorInitializer].getArguments.toList
          args.map(x => astForNode(x))
        } else {
          List()
        }

      callAst(cpgNewExpression, List(cpgTypeId) ++ args)
    }
  }

  private def astForDeleteExpression(delExpression: ICPPASTDeleteExpression): Ast = {
    val cpgDeleteNode =
      newCallNode(delExpression, Operators.delete, Operators.delete, DispatchTypes.STATIC_DISPATCH)
    val arg = astForExpression(delExpression.getOperand)
    callAst(cpgDeleteNode, List(arg))
  }

  private def astForTypeIdInitExpression(typeIdInit: IASTTypeIdInitializerExpression): Ast = {
    val cpgCastExpression =
      newCallNode(typeIdInit, Operators.cast, Operators.cast, DispatchTypes.STATIC_DISPATCH)

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

  private def astForCompoundStatementExpression(compoundExpression: IGNUASTCompoundStatementExpression): Ast =
    nullSafeAst(compoundExpression.getCompoundStatement).headOption.getOrElse(Ast())

  private def astForPackExpansionExpression(packExpansionExpression: ICPPASTPackExpansionExpression): Ast =
    astForExpression(packExpansionExpression.getPattern)

  protected def astForExpression(expression: IASTExpression): Ast = {
    val r = expression match {
      case lit: IASTLiteralExpression   => astForLiteral(lit)
      case un: IASTUnaryExpression      => astForUnaryExpression(un)
      case bin: IASTBinaryExpression    => astForBinaryExpression(bin)
      case exprList: IASTExpressionList => astForExpressionList(exprList)
      case qualId: IASTIdExpression if qualId.getName.isInstanceOf[CPPASTQualifiedName] =>
        astForQualifiedName(qualId.getName.asInstanceOf[CPPASTQualifiedName])
      case ident: IASTIdExpression          => astForIdentifier(ident)
      case call: IASTFunctionCallExpression => astForCallExpression(call)
      case typeId: IASTTypeIdExpression     => astForTypeIdExpression(typeId)
      case fieldRef: IASTFieldReference     => astForFieldReference(fieldRef)
      case expr: IASTConditionalExpression  => astForConditionalExpression(expr)
      case arrayIndexExpression: IASTArraySubscriptExpression =>
        astForArrayIndexExpression(arrayIndexExpression)
      case castExpression: IASTCastExpression          => astForCastExpression(castExpression)
      case newExpression: ICPPASTNewExpression         => astForNewExpression(newExpression)
      case delExpression: ICPPASTDeleteExpression      => astForDeleteExpression(delExpression)
      case typeIdInit: IASTTypeIdInitializerExpression => astForTypeIdInitExpression(typeIdInit)
      case c: ICPPASTSimpleTypeConstructorExpression   => astForConstructorExpression(c)
      case lambdaExpression: ICPPASTLambdaExpression   => astForMethodRefForLambda(lambdaExpression)
      case compoundExpression: IGNUASTCompoundStatementExpression =>
        astForCompoundStatementExpression(compoundExpression)
      case packExpansionExpression: ICPPASTPackExpansionExpression =>
        astForPackExpansionExpression(packExpansionExpression)
      case _ => notHandledYet(expression)
    }
    asChildOfMacroCall(expression, r)
  }

  protected def astForStaticAssert(a: ICPPASTStaticAssertDeclaration): Ast = {
    val name  = "static_assert"
    val call  = newCallNode(a, name, name, DispatchTypes.STATIC_DISPATCH)
    val cond  = nullSafeAst(a.getCondition)
    val messg = nullSafeAst(a.getMessage)
    callAst(call, List(cond, messg))
  }

}
