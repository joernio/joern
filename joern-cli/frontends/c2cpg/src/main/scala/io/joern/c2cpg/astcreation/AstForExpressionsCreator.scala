package io.joern.c2cpg.astcreation

import io.shiftleft.codepropertygraph.generated.nodes.{NewCall, NewIdentifier, NewMethodRef}
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, Operators}
import io.joern.x2cpg.{Ast, ValidationMode}
import org.eclipse.cdt.core.dom.ast.*
import org.eclipse.cdt.core.dom.ast.cpp.*
import org.eclipse.cdt.core.dom.ast.gnu.IGNUASTCompoundStatementExpression
import org.eclipse.cdt.internal.core.dom.parser.cpp.CPPASTQualifiedName

trait AstForExpressionsCreator(implicit withSchemaValidation: ValidationMode) { this: AstCreator =>

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

    val callNode_ = callNode(bin, code(bin), op, op, DispatchTypes.STATIC_DISPATCH)
    val left      = nullSafeAst(bin.getOperand1)
    val right     = nullSafeAst(bin.getOperand2)
    callAst(callNode_, List(left, right))
  }

  private def astForExpressionList(exprList: IASTExpressionList): Ast = {
    val name = "<operator>.expressionList"
    val callNode_ =
      callNode(exprList, code(exprList), name, name, DispatchTypes.STATIC_DISPATCH)
    val childAsts = exprList.getExpressions.map(nullSafeAst)
    callAst(callNode_, childAsts.toIndexedSeq)
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
      case _
          if rec.root.exists(_.isInstanceOf[NewCall]) && call.getFunctionNameExpression
            .isInstanceOf[IASTFieldReference] =>
        (
          DispatchTypes.STATIC_DISPATCH,
          code(call.getFunctionNameExpression.asInstanceOf[IASTFieldReference].getFieldName)
        )
      case _ if rec.root.exists(_.isInstanceOf[NewCall]) =>
        (DispatchTypes.STATIC_DISPATCH, rec.root.get.asInstanceOf[NewCall].code)
      case reference: IASTIdExpression =>
        (DispatchTypes.STATIC_DISPATCH, code(reference))
      case _ =>
        (DispatchTypes.STATIC_DISPATCH, "")
    }

    val shortName = fixQualifiedName(name)
    val fullName = typeFor(call.getFunctionNameExpression) match {
      case t if t == shortName || t.endsWith(s".$shortName") => dereferenceTypeFullName(t)
      case t if t != Defines.anyTypeName                     => s"${dereferenceTypeFullName(t)}.$shortName"
      case _                                                 => shortName
    }
    val cpgCall = callNode(call, code(call), shortName, fullName, dd)
    val args    = call.getArguments.toList.map(a => astForNode(a))
    rec.root match {
      // Optimization: do not include the receiver if the receiver is just the function name,
      // e.g., for `f(x)`, don't include an `f` identifier node as a first child. Since we
      // have so many call sites in CPGs, this drastically reduces the number of nodes.
      // Moreover, the data flow tracker does not need to track `f`, which would not make
      // much sense anyway.
      case Some(r: NewIdentifier) if r.name == shortName =>
        callAst(cpgCall, args)
      case Some(r: NewMethodRef) if r.code == shortName =>
        callAst(cpgCall, args)
      case Some(_) =>
        callAst(cpgCall, args, Option(rec))
      case None =>
        callAst(cpgCall, args)
    }
  }

  private def astForUnaryExpression(unary: IASTUnaryExpression): Ast = {
    val operatorMethod = unary.getOperator match {
      case IASTUnaryExpression.op_prefixIncr       => Operators.preIncrement
      case IASTUnaryExpression.op_prefixDecr       => Operators.preDecrement
      case IASTUnaryExpression.op_plus             => Operators.plus
      case IASTUnaryExpression.op_minus            => Operators.minus
      case IASTUnaryExpression.op_star             => Operators.indirection
      case IASTUnaryExpression.op_amper            => Operators.addressOf
      case IASTUnaryExpression.op_tilde            => Operators.not
      case IASTUnaryExpression.op_not              => Operators.logicalNot
      case IASTUnaryExpression.op_sizeof           => Operators.sizeOf
      case IASTUnaryExpression.op_postFixIncr      => Operators.postIncrement
      case IASTUnaryExpression.op_postFixDecr      => Operators.postDecrement
      case IASTUnaryExpression.op_throw            => "<operator>.throw"
      case IASTUnaryExpression.op_typeid           => "<operator>.typeOf"
      case IASTUnaryExpression.op_bracketedPrimary => "<operator>.bracketedPrimary"
      case _                                       => "<operator>.unknown"
    }

    if (
      unary.getOperator == IASTUnaryExpression.op_bracketedPrimary &&
      !unary.getOperand.isInstanceOf[IASTExpressionList]
    ) {
      nullSafeAst(unary.getOperand)
    } else {
      val cpgUnary =
        callNode(unary, code(unary), operatorMethod, operatorMethod, DispatchTypes.STATIC_DISPATCH)
      val operand = nullSafeAst(unary.getOperand)
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
        val call =
          callNode(typeId, code(typeId), Operators.sizeOf, Operators.sizeOf, DispatchTypes.STATIC_DISPATCH)
        val arg = astForNode(typeId.getTypeId.getDeclSpecifier)
        callAst(call, List(arg))
      case _ => notHandledYet(typeId)
    }
  }

  private def astForConditionalExpression(expr: IASTConditionalExpression): Ast = {
    val name = Operators.conditional
    val call = callNode(expr, code(expr), name, name, DispatchTypes.STATIC_DISPATCH)

    val condAst = nullSafeAst(expr.getLogicalConditionExpression)
    val posAst  = nullSafeAst(expr.getPositiveResultExpression)
    val negAst  = nullSafeAst(expr.getNegativeResultExpression)

    val children = List(condAst, posAst, negAst)
    callAst(call, children)
  }

  private def astForArrayIndexExpression(arrayIndexExpression: IASTArraySubscriptExpression): Ast = {
    val name = Operators.indirectIndexAccess
    val cpgArrayIndexing =
      callNode(arrayIndexExpression, code(arrayIndexExpression), name, name, DispatchTypes.STATIC_DISPATCH)

    val expr = astForExpression(arrayIndexExpression.getArrayExpression)
    val arg  = astForNode(arrayIndexExpression.getArgument)
    callAst(cpgArrayIndexing, List(expr, arg))
  }

  private def astForCastExpression(castExpression: IASTCastExpression): Ast = {
    val cpgCastExpression =
      callNode(castExpression, code(castExpression), Operators.cast, Operators.cast, DispatchTypes.STATIC_DISPATCH)

    val expr    = astForExpression(castExpression.getOperand)
    val argNode = castExpression.getTypeId
    val arg     = unknownNode(argNode, code(argNode))

    callAst(cpgCastExpression, List(Ast(arg), expr))
  }

  private def astsForConstructorInitializer(initializer: IASTInitializer): List[Ast] = {
    initializer match {
      case init: ICPPASTConstructorInitializer => init.getArguments.toList.map(x => astForNode(x))
      case _                                   => Nil // null or unexpected type
    }
  }

  private def astsForInitializerPlacements(initializerPlacements: Array[IASTInitializerClause]): List[Ast] = {
    if (initializerPlacements != null) initializerPlacements.toList.map(x => astForNode(x))
    else Nil
  }

  private def astForNewExpression(newExpression: ICPPASTNewExpression): Ast = {
    val name = "<operator>.new"
    val cpgNewExpression =
      callNode(newExpression, code(newExpression), name, name, DispatchTypes.STATIC_DISPATCH)

    val typeId = newExpression.getTypeId
    if (newExpression.isArrayAllocation) {
      val cpgTypeId = astForIdentifier(typeId.getDeclSpecifier)
      Ast(cpgNewExpression).withChild(cpgTypeId).withArgEdge(cpgNewExpression, cpgTypeId.root.get)
    } else {
      val cpgTypeId = astForIdentifier(typeId.getDeclSpecifier)
      val args = astsForConstructorInitializer(newExpression.getInitializer) ++
        astsForInitializerPlacements(newExpression.getPlacementArguments)
      callAst(cpgNewExpression, List(cpgTypeId) ++ args)
    }
  }

  private def astForDeleteExpression(delExpression: ICPPASTDeleteExpression): Ast = {
    val name = Operators.delete
    val cpgDeleteNode =
      callNode(delExpression, code(delExpression), name, name, DispatchTypes.STATIC_DISPATCH)
    val arg = astForExpression(delExpression.getOperand)
    callAst(cpgDeleteNode, List(arg))
  }

  private def astForTypeIdInitExpression(typeIdInit: IASTTypeIdInitializerExpression): Ast = {
    val name = Operators.cast
    val cpgCastExpression =
      callNode(typeIdInit, code(typeIdInit), name, name, DispatchTypes.STATIC_DISPATCH)

    val typeAst = unknownNode(typeIdInit.getTypeId, code(typeIdInit.getTypeId))
    val expr    = astForNode(typeIdInit.getInitializer)
    callAst(cpgCastExpression, List(Ast(typeAst), expr))
  }

  private def astForConstructorExpression(c: ICPPASTSimpleTypeConstructorExpression): Ast = {
    val name      = c.getDeclSpecifier.toString
    val callNode_ = callNode(c, code(c), name, name, DispatchTypes.STATIC_DISPATCH)
    val arg       = astForNode(c.getInitializer)
    callAst(callNode_, List(arg))
  }

  private def astForCompoundStatementExpression(compoundExpression: IGNUASTCompoundStatementExpression): Ast =
    nullSafeAst(compoundExpression.getCompoundStatement).headOption.getOrElse(Ast())

  private def astForPackExpansionExpression(packExpansionExpression: ICPPASTPackExpansionExpression): Ast =
    astForExpression(packExpansionExpression.getPattern)

  protected def astForExpression(expression: IASTExpression): Ast = {
    val r = expression match {
      case lit: IASTLiteralExpression                  => astForLiteral(lit)
      case un: IASTUnaryExpression                     => astForUnaryExpression(un)
      case bin: IASTBinaryExpression                   => astForBinaryExpression(bin)
      case exprList: IASTExpressionList                => astForExpressionList(exprList)
      case idExpr: IASTIdExpression                    => astForIdExpression(idExpr)
      case call: IASTFunctionCallExpression            => astForCallExpression(call)
      case typeId: IASTTypeIdExpression                => astForTypeIdExpression(typeId)
      case fieldRef: IASTFieldReference                => astForFieldReference(fieldRef)
      case expr: IASTConditionalExpression             => astForConditionalExpression(expr)
      case arr: IASTArraySubscriptExpression           => astForArrayIndexExpression(arr)
      case castExpression: IASTCastExpression          => astForCastExpression(castExpression)
      case newExpression: ICPPASTNewExpression         => astForNewExpression(newExpression)
      case delExpression: ICPPASTDeleteExpression      => astForDeleteExpression(delExpression)
      case typeIdInit: IASTTypeIdInitializerExpression => astForTypeIdInitExpression(typeIdInit)
      case c: ICPPASTSimpleTypeConstructorExpression   => astForConstructorExpression(c)
      case lambdaExpression: ICPPASTLambdaExpression   => astForMethodRefForLambda(lambdaExpression)
      case cExpr: IGNUASTCompoundStatementExpression   => astForCompoundStatementExpression(cExpr)
      case pExpr: ICPPASTPackExpansionExpression       => astForPackExpansionExpression(pExpr)
      case _                                           => notHandledYet(expression)
    }
    asChildOfMacroCall(expression, r)
  }

  private def astForIdExpression(idExpression: IASTIdExpression): Ast = idExpression.getName match {
    case name: CPPASTQualifiedName => astForQualifiedName(name)
    case _                         => astForIdentifier(idExpression)
  }

  protected def astForStaticAssert(a: ICPPASTStaticAssertDeclaration): Ast = {
    val name  = "static_assert"
    val call  = callNode(a, code(a), name, name, DispatchTypes.STATIC_DISPATCH)
    val cond  = nullSafeAst(a.getCondition)
    val messg = nullSafeAst(a.getMessage)
    callAst(call, List(cond, messg))
  }

}
