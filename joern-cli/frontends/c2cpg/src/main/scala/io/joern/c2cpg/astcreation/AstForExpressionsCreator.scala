package io.joern.c2cpg.astcreation

import io.joern.x2cpg.Ast
import io.joern.x2cpg.ValidationMode
import io.joern.x2cpg.Defines as X2CpgDefines
import io.shiftleft.codepropertygraph.generated.DispatchTypes
import io.shiftleft.codepropertygraph.generated.Operators
import org.eclipse.cdt.core.dom.ast
import org.eclipse.cdt.core.dom.ast.*
import org.eclipse.cdt.core.dom.ast.cpp.*
import org.eclipse.cdt.core.dom.ast.gnu.IGNUASTCompoundStatementExpression
import org.eclipse.cdt.internal.core.dom.parser.c.CASTFunctionCallExpression
import org.eclipse.cdt.internal.core.dom.parser.c.CASTIdExpression
import org.eclipse.cdt.internal.core.dom.parser.c.CFunctionType
import org.eclipse.cdt.internal.core.dom.parser.c.CPointerType
import org.eclipse.cdt.internal.core.dom.parser.cpp.CPPASTIdExpression
import org.eclipse.cdt.internal.core.dom.parser.cpp.CPPASTQualifiedName
import org.eclipse.cdt.internal.core.dom.parser.cpp.CPPClosureType
import org.eclipse.cdt.internal.core.dom.parser.cpp.semantics.EvalFunctionCall

import scala.util.Try

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
      case IASTBinaryExpression.op_max              => Defines.OperatorMax
      case IASTBinaryExpression.op_min              => Defines.OperatorMin
      case IASTBinaryExpression.op_ellipses         => Defines.OperatorEllipses
      case _                                        => Defines.OperatorUnknown
    }

    val callNode_ = callNode(bin, code(bin), op, op, DispatchTypes.STATIC_DISPATCH, None, Some(X2CpgDefines.Any))
    val left      = nullSafeAst(bin.getOperand1)
    val right     = nullSafeAst(bin.getOperand2)
    callAst(callNode_, List(left, right))
  }

  private def astForExpressionList(exprList: IASTExpressionList): Ast = {
    val name = Defines.OperatorExpressionList
    val callNode_ =
      callNode(exprList, code(exprList), name, name, DispatchTypes.STATIC_DISPATCH, None, Some(X2CpgDefines.Any))
    val childAsts = exprList.getExpressions.map(nullSafeAst)
    callAst(callNode_, childAsts.toIndexedSeq)
  }

  private def astForCppCallExpression(call: ICPPASTFunctionCallExpression): Ast = {
    val functionNameExpr = call.getFunctionNameExpression
    val typ              = functionNameExpr.getExpressionType
    typ match {
      case pointerType: IPointerType =>
        createPointerCallAst(call, cleanType(safeGetType(call.getExpressionType)))
      case functionType: ICPPFunctionType =>
        functionNameExpr match {
          case idExpr: CPPASTIdExpression if idExpr.getName.getBinding.isInstanceOf[ICPPFunction] =>
            val function = idExpr.getName.getBinding.asInstanceOf[ICPPFunction]
            val name     = idExpr.getName.getLastName.toString
            val signature =
              if (function.isExternC) {
                ""
              } else {
                functionTypeToSignature(functionType)
              }

            val fullName =
              if (function.isExternC) {
                name
              } else {
                val fullNameNoSig = function.getQualifiedName.mkString(".")
                s"$fullNameNoSig:$signature"
              }

            val dispatchType = DispatchTypes.STATIC_DISPATCH

            val callCpgNode = callNode(
              call,
              code(call),
              name,
              fullName,
              dispatchType,
              Some(signature),
              Some(registerType(cleanType(safeGetType(call.getExpressionType))))
            )
            val args = call.getArguments.toList.map(a => astForNode(a))

            createCallAst(callCpgNode, args)
          case fieldRefExpr: ICPPASTFieldReference =>
            val instanceAst = astForExpression(fieldRefExpr.getFieldOwner)
            val args        = call.getArguments.toList.map(a => astForNode(a))

            // TODO This wont do if the name is a reference.
            val name      = fieldRefExpr.getFieldName.toString
            val signature = functionTypeToSignature(functionType)

            val classFullName = cleanType(safeGetType(fieldRefExpr.getFieldOwnerType))
            val fullName      = s"$classFullName.$name:$signature"

            fieldRefExpr.getFieldName.resolveBinding()
            val method = fieldRefExpr.getFieldName.getBinding.asInstanceOf[ICPPMethod]
            val (dispatchType, receiver) =
              if (method.isVirtual || method.isPureVirtual) {
                (DispatchTypes.DYNAMIC_DISPATCH, Some(instanceAst))
              } else {
                (DispatchTypes.STATIC_DISPATCH, None)
              }
            val callCpgNode = callNode(
              call,
              code(call),
              name,
              fullName,
              dispatchType,
              Some(signature),
              Some(registerType(cleanType(safeGetType(call.getExpressionType))))
            )
            createCallAst(callCpgNode, args, base = Some(instanceAst), receiver)
          case other =>
            astForCppCallExpressionUntyped(call)
        }
      case classType: ICPPClassType =>
        val evaluation = call.getEvaluation.asInstanceOf[EvalFunctionCall]

        val functionType = Try(evaluation.getOverload.getType).toOption
        val signature    = functionType.map(functionTypeToSignature).getOrElse(X2CpgDefines.UnresolvedSignature)
        val name         = Defines.OperatorCall

        classType match {
          case closureType: CPPClosureType =>
            val fullName     = s"$name:$signature"
            val dispatchType = DispatchTypes.DYNAMIC_DISPATCH

            val callCpgNode = callNode(
              call,
              code(call),
              name,
              fullName,
              dispatchType,
              Some(signature),
              Some(registerType(cleanType(safeGetType(call.getExpressionType))))
            )

            val receiverAst = astForExpression(functionNameExpr)
            val args        = call.getArguments.toList.map(a => astForNode(a))

            createCallAst(callCpgNode, args, receiver = Some(receiverAst))
          case _ =>
            val classFullName = cleanType(safeGetType(classType))
            val fullName      = s"$classFullName.$name:$signature"

            val dispatchType = evaluation.getOverload match {
              case method: ICPPMethod =>
                if (method.isVirtual || method.isPureVirtual) {
                  DispatchTypes.DYNAMIC_DISPATCH
                } else {
                  DispatchTypes.STATIC_DISPATCH
                }
              case _ =>
                DispatchTypes.STATIC_DISPATCH
            }
            val callCpgNode = callNode(
              call,
              code(call),
              name,
              fullName,
              dispatchType,
              Some(signature),
              Some(registerType(cleanType(safeGetType(call.getExpressionType))))
            )

            val instanceAst = astForExpression(functionNameExpr)
            val args        = call.getArguments.toList.map(a => astForNode(a))
            createCallAst(callCpgNode, args, base = Some(instanceAst), receiver = Some(instanceAst))
        }
      case _: IProblemType =>
        astForCppCallExpressionUntyped(call)
      case _: IProblemBinding =>
        astForCppCallExpressionUntyped(call)
      case other =>
        astForCppCallExpressionUntyped(call)
    }
  }

  private def astForCppCallExpressionUntyped(call: ICPPASTFunctionCallExpression): Ast = {
    val functionNameExpr = call.getFunctionNameExpression

    functionNameExpr match {
      case fieldRefExpr: ICPPASTFieldReference =>
        val instanceAst = astForExpression(fieldRefExpr.getFieldOwner)
        val args        = call.getArguments.toList.map(a => astForNode(a))

        val name      = fieldRefExpr.getFieldName.toString
        val signature = X2CpgDefines.UnresolvedSignature
        val fullName  = s"${X2CpgDefines.UnresolvedNamespace}.$name:$signature(${args.size})"

        val callCpgNode = callNode(
          call,
          code(call),
          name,
          fullName,
          DispatchTypes.STATIC_DISPATCH,
          Some(signature),
          Some(X2CpgDefines.Any)
        )
        createCallAst(callCpgNode, args, base = Some(instanceAst), receiver = Some(instanceAst))
      case idExpr: CPPASTIdExpression =>
        val args = call.getArguments.toList.map(a => astForNode(a))

        val name      = idExpr.getName.getLastName.toString
        val signature = X2CpgDefines.UnresolvedSignature
        val fullName  = s"${X2CpgDefines.UnresolvedNamespace}.$name:$signature(${args.size})"

        val callCpgNode = callNode(
          call,
          code(call),
          name,
          fullName,
          DispatchTypes.STATIC_DISPATCH,
          Some(signature),
          Some(X2CpgDefines.Any)
        )
        createCallAst(callCpgNode, args)
      case other =>
        // This could either be a pointer or an operator() call we dont know at this point
        // but since it is CPP we opt for the later.
        val args = call.getArguments.toList.map(a => astForNode(a))

        val name      = Defines.OperatorCall
        val signature = X2CpgDefines.UnresolvedSignature
        val fullName  = s"${X2CpgDefines.UnresolvedNamespace}.$name:$signature(${args.size})"

        val callCpgNode = callNode(
          call,
          code(call),
          name,
          fullName,
          DispatchTypes.STATIC_DISPATCH,
          Some(signature),
          Some(X2CpgDefines.Any)
        )
        val instanceAst = astForExpression(functionNameExpr)
        createCallAst(callCpgNode, args, base = Some(instanceAst), receiver = Some(instanceAst))
    }
  }

  private def astForCCallExpression(call: CASTFunctionCallExpression): Ast = {
    val functionNameExpr = call.getFunctionNameExpression
    val typ              = functionNameExpr.getExpressionType
    typ match {
      case pointerType: CPointerType =>
        createPointerCallAst(call, cleanType(safeGetType(call.getExpressionType)))
      case functionType: CFunctionType =>
        functionNameExpr match {
          case idExpr: CASTIdExpression =>
            createCFunctionCallAst(call, idExpr, cleanType(safeGetType(call.getExpressionType)))
          case _ =>
            createPointerCallAst(call, cleanType(safeGetType(call.getExpressionType)))
        }
      case _ =>
        astForCCallExpressionUntyped(call)
    }
  }

  private def createCFunctionCallAst(
    call: CASTFunctionCallExpression,
    idExpr: CASTIdExpression,
    callTypeFullName: String
  ): Ast = {
    val name         = idExpr.getName.getLastName.toString
    val signature    = ""
    val dispatchType = DispatchTypes.STATIC_DISPATCH
    val callCpgNode =
      callNode(call, code(call), name, name, dispatchType, Some(signature), Some(registerType(callTypeFullName)))
    val args = call.getArguments.toList.map(a => astForNode(a))
    createCallAst(callCpgNode, args)
  }

  private def createPointerCallAst(call: IASTFunctionCallExpression, callTypeFullName: String): Ast = {
    val functionNameExpr = call.getFunctionNameExpression
    val name             = Defines.OperatorPointerCall
    val signature        = ""
    val dispatchType     = DispatchTypes.DYNAMIC_DISPATCH
    val callCpgNode =
      callNode(call, code(call), name, name, dispatchType, Some(signature), Some(registerType(callTypeFullName)))
    val args        = call.getArguments.toList.map(a => astForNode(a))
    val receiverAst = astForExpression(functionNameExpr)
    createCallAst(callCpgNode, args, receiver = Some(receiverAst))
  }

  private def astForCCallExpressionUntyped(call: CASTFunctionCallExpression): Ast = {
    val functionNameExpr = call.getFunctionNameExpression
    functionNameExpr match {
      case idExpr: CASTIdExpression => createCFunctionCallAst(call, idExpr, X2CpgDefines.Any)
      case _                        => createPointerCallAst(call, X2CpgDefines.Any)
    }
  }

  private def astForCallExpression(call: IASTFunctionCallExpression): Ast = {
    call match {
      case cppCall: ICPPASTFunctionCallExpression => astForCppCallExpression(cppCall)
      case cCall: CASTFunctionCallExpression      => astForCCallExpression(cCall)
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
      case IASTUnaryExpression.op_throw            => Defines.OperatorThrow
      case IASTUnaryExpression.op_typeid           => Defines.OperatorTypeOf
      case IASTUnaryExpression.op_bracketedPrimary => Defines.OperatorBracketedPrimary
      case _                                       => Defines.OperatorUnknown
    }

    if (
      unary.getOperator == IASTUnaryExpression.op_bracketedPrimary &&
      !unary.getOperand.isInstanceOf[IASTExpressionList]
    ) {
      nullSafeAst(unary.getOperand)
    } else {
      val cpgUnary = callNode(
        unary,
        code(unary),
        operatorMethod,
        operatorMethod,
        DispatchTypes.STATIC_DISPATCH,
        None,
        Some(X2CpgDefines.Any)
      )
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
          callNode(
            typeId,
            code(typeId),
            Operators.sizeOf,
            Operators.sizeOf,
            DispatchTypes.STATIC_DISPATCH,
            None,
            Some(X2CpgDefines.Any)
          )
        val arg = astForNode(typeId.getTypeId.getDeclSpecifier)
        callAst(call, List(arg))
      case _ => notHandledYet(typeId)
    }
  }

  private def astForConditionalExpression(expr: IASTConditionalExpression): Ast = {
    val name = Operators.conditional
    val call = callNode(expr, code(expr), name, name, DispatchTypes.STATIC_DISPATCH, None, Some(X2CpgDefines.Any))

    val condAst = nullSafeAst(expr.getLogicalConditionExpression)
    val posAst  = nullSafeAst(expr.getPositiveResultExpression)
    val negAst  = nullSafeAst(expr.getNegativeResultExpression)

    val children = List(condAst, posAst, negAst)
    callAst(call, children)
  }

  private def astForArrayIndexExpression(arrayIndexExpression: IASTArraySubscriptExpression): Ast = {
    val name = Operators.indirectIndexAccess
    val cpgArrayIndexing =
      callNode(
        arrayIndexExpression,
        code(arrayIndexExpression),
        name,
        name,
        DispatchTypes.STATIC_DISPATCH,
        None,
        Some(X2CpgDefines.Any)
      )

    val expr = astForExpression(arrayIndexExpression.getArrayExpression)
    val arg  = astForNode(arrayIndexExpression.getArgument)
    callAst(cpgArrayIndexing, List(expr, arg))
  }

  private def astForCastExpression(castExpression: IASTCastExpression): Ast = {
    val cpgCastExpression =
      callNode(
        castExpression,
        code(castExpression),
        Operators.cast,
        Operators.cast,
        DispatchTypes.STATIC_DISPATCH,
        None,
        Some(X2CpgDefines.Any)
      )

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
    val name = Defines.OperatorNew
    val cpgNewExpression = callNode(
      newExpression,
      code(newExpression),
      name,
      name,
      DispatchTypes.STATIC_DISPATCH,
      None,
      Some(X2CpgDefines.Any)
    )

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
      callNode(
        delExpression,
        code(delExpression),
        name,
        name,
        DispatchTypes.STATIC_DISPATCH,
        None,
        Some(X2CpgDefines.Any)
      )
    val arg = astForExpression(delExpression.getOperand)
    callAst(cpgDeleteNode, List(arg))
  }

  private def astForTypeIdInitExpression(typeIdInit: IASTTypeIdInitializerExpression): Ast = {
    val name = Operators.cast
    val cpgCastExpression =
      callNode(typeIdInit, code(typeIdInit), name, name, DispatchTypes.STATIC_DISPATCH, None, Some(X2CpgDefines.Any))

    val typeAst = unknownNode(typeIdInit.getTypeId, code(typeIdInit.getTypeId))
    val expr    = astForNode(typeIdInit.getInitializer)
    callAst(cpgCastExpression, List(Ast(typeAst), expr))
  }

  private def astForConstructorExpression(c: ICPPASTSimpleTypeConstructorExpression): Ast = {
    val name      = c.getDeclSpecifier.toString
    val callNode_ = callNode(c, code(c), name, name, DispatchTypes.STATIC_DISPATCH, None, Some(X2CpgDefines.Any))
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
    val name    = "static_assert"
    val call    = callNode(a, code(a), name, name, DispatchTypes.STATIC_DISPATCH, None, Some(X2CpgDefines.Any))
    val cond    = nullSafeAst(a.getCondition)
    val message = nullSafeAst(a.getMessage)
    callAst(call, List(cond, message))
  }

}
