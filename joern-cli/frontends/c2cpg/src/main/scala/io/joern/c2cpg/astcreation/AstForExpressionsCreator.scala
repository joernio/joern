package io.joern.c2cpg.astcreation

import io.shiftleft.codepropertygraph.generated.nodes.{NewBlock, NewCall, NewIdentifier, NewMethodRef}
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, Operators}
import io.shiftleft.x2cpg.Ast
import org.eclipse.cdt.core.dom.ast._
import org.eclipse.cdt.core.dom.ast.cpp._
import org.eclipse.cdt.core.dom.ast.gnu.IGNUASTCompoundStatementExpression
import org.eclipse.cdt.internal.core.dom.parser.cpp.CPPASTQualifiedName

trait AstForExpressionsCreator {

  this: AstCreator =>

  import AstCreatorHelper.OptionSafeAst

  private def astForBinaryExpression(bin: IASTBinaryExpression, order: Int): Ast = {
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
    val callNode = newCallNode(bin, op, op, DispatchTypes.STATIC_DISPATCH, order)
    val left     = nullSafeAst(bin.getOperand1, 1)
    val right    = nullSafeAst(bin.getOperand2, 2)
    Ast(callNode)
      .withChild(left)
      .withChild(right)
      .withArgEdge(callNode, left.root)
      .withArgEdge(callNode, right.root)
  }

  private def astForExpressionList(exprList: IASTExpressionList, order: Int): Ast = {
    val b = NewBlock()
      .order(order)
      .argumentIndex(order)
      .typeFullName(registerType(Defines.voidTypeName))
      .lineNumber(line(exprList))
      .columnNumber(column(exprList))
    Ast(b).withChildren(exprList.getExpressions.toIndexedSeq.map(astForExpression(_, order)))
  }

  private def astForCallExpression(call: IASTFunctionCallExpression, order: Int): Ast = {
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
        methodRefAst.root.get.asInstanceOf[NewMethodRef].order = 0
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

    val cpgCall = Ast(newCallNode(call, name, name, dd, order))
    val args    = withOrder(call.getArguments) { case (a, o) => astForNode(a, o) }
    rec.root match {
      // Optimization: do not include the receiver if the receiver is just the function name,
      // e.g., for `f(x)`, don't include an `f` identifier node as a first child. Since we
      // have so many call sites in CPGs, this drastically reduces the number of nodes.
      // Moreover, the data flow tracker does not need to track `f`, which would not make
      // much sense anyway.
      case Some(r: NewIdentifier) if r.name == name =>
        cpgCall.withChildren(args).withArgEdges(cpgCall.root.get, args)
      case Some(r) =>
        cpgCall
          .withChild(rec)
          .withChildren(args)
          .withArgEdges(cpgCall.root.get, args)
          .withReceiverEdge(cpgCall.root.get, r)
      case None => cpgCall.withChildren(args).withArgEdges(cpgCall.root.get, args)
    }
  }

  private def astForUnaryExpression(unary: IASTUnaryExpression, order: Int): Ast = {
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
      astForExpression(unary.getOperand, order)
    } else {
      val cpgUnary = newCallNode(unary, operatorMethod, operatorMethod, DispatchTypes.STATIC_DISPATCH, order)
      val operandExpr = unary.getOperand match {
        // special handling for operand expression in brackets - we simply ignore the brackets
        case opExpr: IASTUnaryExpression if opExpr.getOperator == IASTUnaryExpression.op_bracketedPrimary =>
          opExpr.getOperand
        case opExpr => opExpr
      }
      val operand = nullSafeAst(operandExpr, 1)
      Ast(cpgUnary).withChild(operand).withArgEdge(cpgUnary, operand.root)
    }
  }

  private def astForTypeIdExpression(typeId: IASTTypeIdExpression, order: Int): Ast = {
    typeId.getOperator match {
      case op
          if op == IASTTypeIdExpression.op_sizeof ||
            op == IASTTypeIdExpression.op_sizeofParameterPack ||
            op == IASTTypeIdExpression.op_typeid ||
            op == IASTTypeIdExpression.op_alignof ||
            op == IASTTypeIdExpression.op_typeof =>
        val call = newCallNode(typeId, Operators.sizeOf, Operators.sizeOf, DispatchTypes.STATIC_DISPATCH, order)
        val arg  = astForNode(typeId.getTypeId.getDeclSpecifier, 1)
        Ast(call).withChild(arg).withArgEdge(call, arg.root)
      case _ => notHandledYet(typeId, order)
    }
  }

  private def astForConditionalExpression(expr: IASTConditionalExpression, order: Int): Ast = {
    val call = newCallNode(expr, Operators.conditional, Operators.conditional, DispatchTypes.STATIC_DISPATCH, order)

    val condAst = nullSafeAst(expr.getLogicalConditionExpression, 1)
    val posAst  = nullSafeAst(expr.getPositiveResultExpression, 2)
    val negAst  = nullSafeAst(expr.getNegativeResultExpression, 3)

    val children = Seq(condAst, posAst, negAst)
    Ast(call).withChildren(children).withArgEdges(call, children)
  }

  private def astForArrayIndexExpression(arrayIndexExpression: IASTArraySubscriptExpression, order: Int): Ast = {
    val cpgArrayIndexing =
      newCallNode(
        arrayIndexExpression,
        Operators.indirectIndexAccess,
        Operators.indirectIndexAccess,
        DispatchTypes.STATIC_DISPATCH,
        order
      )

    val expr = astForExpression(arrayIndexExpression.getArrayExpression, 1)
    val arg  = astForNode(arrayIndexExpression.getArgument, 2)

    Ast(cpgArrayIndexing)
      .withChild(expr)
      .withChild(arg)
      .withArgEdge(cpgArrayIndexing, expr.root)
      .withArgEdge(cpgArrayIndexing, arg.root)
  }

  private def astForCastExpression(castExpression: IASTCastExpression, order: Int): Ast = {
    val cpgCastExpression =
      newCallNode(castExpression, Operators.cast, Operators.cast, DispatchTypes.STATIC_DISPATCH, order)

    val expr    = astForExpression(castExpression.getOperand, 2)
    val argNode = castExpression.getTypeId
    val arg     = newUnknown(argNode, 1)

    Ast(cpgCastExpression)
      .withChild(Ast(arg))
      .withChild(expr)
      .withArgEdge(cpgCastExpression, arg)
      .withArgEdge(cpgCastExpression, expr.root)
  }

  private def astForNewExpression(newExpression: ICPPASTNewExpression, order: Int): Ast = {
    val cpgNewExpression =
      newCallNode(newExpression, "<operator>.new", "<operator>.new", DispatchTypes.STATIC_DISPATCH, order)

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
          withOrder(args) { (a, o) =>
            astForNode(a, 1 + o)
          }
        } else {
          Seq.empty
        }
      Ast(cpgNewExpression)
        .withChild(cpgTypeId)
        .withChildren(args)
        .withArgEdge(cpgNewExpression, cpgTypeId.root.get)
        .withArgEdges(cpgNewExpression, args)
    }
  }

  private def astForDeleteExpression(delExpression: ICPPASTDeleteExpression, order: Int): Ast = {
    val cpgDeleteNode =
      newCallNode(delExpression, Operators.delete, Operators.delete, DispatchTypes.STATIC_DISPATCH, order)
    val arg = astForExpression(delExpression.getOperand, 1)
    Ast(cpgDeleteNode)
      .withChild(arg)
      .withArgEdge(cpgDeleteNode, arg.root)
  }

  private def astForTypeIdInitExpression(typeIdInit: IASTTypeIdInitializerExpression, order: Int): Ast = {
    val cpgCastExpression =
      newCallNode(typeIdInit, Operators.cast, Operators.cast, DispatchTypes.STATIC_DISPATCH, order)

    val typeAst = newUnknown(typeIdInit.getTypeId, 1)
    val expr    = astForNode(typeIdInit.getInitializer, 2)

    Ast(cpgCastExpression)
      .withChild(Ast(typeAst))
      .withChild(expr)
      .withArgEdge(cpgCastExpression, typeAst)
      .withArgEdge(cpgCastExpression, expr.root)
  }

  private def astForConstructorExpression(c: ICPPASTSimpleTypeConstructorExpression, order: Int): Ast = {
    val name     = c.getDeclSpecifier.toString
    val callNode = newCallNode(c, name, name, DispatchTypes.STATIC_DISPATCH, order)
    val arg      = astForNode(c.getInitializer, 1)
    Ast(callNode).withChild(arg).withArgEdge(callNode, arg.root)
  }

  private def astForCompoundStatementExpression(
    compoundExpression: IGNUASTCompoundStatementExpression,
    order: Int
  ): Ast =
    nullSafeAst(compoundExpression.getCompoundStatement, order).headOption.getOrElse(Ast())

  private def astForPackExpansionExpression(packExpansionExpression: ICPPASTPackExpansionExpression, order: Int): Ast =
    astForExpression(packExpansionExpression.getPattern, order)

  protected def astForExpression(expression: IASTExpression, order: Int): Ast = {
    val r = expression match {
      case lit: IASTLiteralExpression   => astForLiteral(lit, order)
      case un: IASTUnaryExpression      => astForUnaryExpression(un, order)
      case bin: IASTBinaryExpression    => astForBinaryExpression(bin, order)
      case exprList: IASTExpressionList => astForExpressionList(exprList, order)
      case qualId: IASTIdExpression if qualId.getName.isInstanceOf[CPPASTQualifiedName] =>
        astForQualifiedName(qualId.getName.asInstanceOf[CPPASTQualifiedName], order)
      case ident: IASTIdExpression                            => astForIdentifier(ident, order)
      case call: IASTFunctionCallExpression                   => astForCallExpression(call, order)
      case typeId: IASTTypeIdExpression                       => astForTypeIdExpression(typeId, order)
      case fieldRef: IASTFieldReference                       => astForFieldReference(fieldRef, order)
      case expr: IASTConditionalExpression                    => astForConditionalExpression(expr, order)
      case arrayIndexExpression: IASTArraySubscriptExpression => astForArrayIndexExpression(arrayIndexExpression, order)
      case castExpression: IASTCastExpression                 => astForCastExpression(castExpression, order)
      case newExpression: ICPPASTNewExpression                => astForNewExpression(newExpression, order)
      case delExpression: ICPPASTDeleteExpression             => astForDeleteExpression(delExpression, order)
      case typeIdInit: IASTTypeIdInitializerExpression        => astForTypeIdInitExpression(typeIdInit, order)
      case c: ICPPASTSimpleTypeConstructorExpression          => astForConstructorExpression(c, order)
      case lambdaExpression: ICPPASTLambdaExpression          => astForMethodRefForLambda(lambdaExpression)
      case compoundExpression: IGNUASTCompoundStatementExpression =>
        astForCompoundStatementExpression(compoundExpression, order)
      case packExpansionExpression: ICPPASTPackExpansionExpression =>
        astForPackExpansionExpression(packExpansionExpression, order)
      case _ => notHandledYet(expression, order)
    }
    asChildOfMacroCall(expression, r, order)
  }

  protected def astForStaticAssert(a: ICPPASTStaticAssertDeclaration, order: Int): Ast = {
    val name  = "static_assert"
    val call  = newCallNode(a, name, name, DispatchTypes.STATIC_DISPATCH, order)
    val cond  = nullSafeAst(a.getCondition, 1)
    val messg = nullSafeAst(a.getMessage, 2)
    var ast   = Ast(call).withChild(cond).withChild(messg)
    cond.root.foreach(r => ast = ast.withArgEdge(call, r))
    messg.root.foreach(m => ast = ast.withArgEdge(call, m))
    ast
  }

}
