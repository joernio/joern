package io.joern.c2cpg.astcreation

import io.joern.x2cpg.Ast
import io.joern.x2cpg.ValidationMode
import io.joern.x2cpg.Defines as X2CpgDefines
import io.shiftleft.codepropertygraph.generated.{ControlStructureTypes, DispatchTypes, Operators}
import org.apache.commons.lang3.StringUtils
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
import org.eclipse.cdt.internal.core.dom.parser.cpp.CPPASTFoldExpression

import scala.util.Try

trait AstForExpressionsCreator(implicit withSchemaValidation: ValidationMode) { this: AstCreator =>

  private val OperatorMap: Map[Int, String] = Map(
    IASTBinaryExpression.op_multiply         -> Operators.multiplication,
    IASTBinaryExpression.op_divide           -> Operators.division,
    IASTBinaryExpression.op_modulo           -> Operators.modulo,
    IASTBinaryExpression.op_plus             -> Operators.addition,
    IASTBinaryExpression.op_minus            -> Operators.subtraction,
    IASTBinaryExpression.op_shiftLeft        -> Operators.shiftLeft,
    IASTBinaryExpression.op_shiftRight       -> Operators.arithmeticShiftRight,
    IASTBinaryExpression.op_lessThan         -> Operators.lessThan,
    IASTBinaryExpression.op_greaterThan      -> Operators.greaterThan,
    IASTBinaryExpression.op_lessEqual        -> Operators.lessEqualsThan,
    IASTBinaryExpression.op_greaterEqual     -> Operators.greaterEqualsThan,
    IASTBinaryExpression.op_binaryAnd        -> Operators.and,
    IASTBinaryExpression.op_binaryXor        -> Operators.xor,
    IASTBinaryExpression.op_binaryOr         -> Operators.or,
    IASTBinaryExpression.op_logicalAnd       -> Operators.logicalAnd,
    IASTBinaryExpression.op_logicalOr        -> Operators.logicalOr,
    IASTBinaryExpression.op_assign           -> Operators.assignment,
    IASTBinaryExpression.op_multiplyAssign   -> Operators.assignmentMultiplication,
    IASTBinaryExpression.op_divideAssign     -> Operators.assignmentDivision,
    IASTBinaryExpression.op_moduloAssign     -> Operators.assignmentModulo,
    IASTBinaryExpression.op_plusAssign       -> Operators.assignmentPlus,
    IASTBinaryExpression.op_minusAssign      -> Operators.assignmentMinus,
    IASTBinaryExpression.op_shiftLeftAssign  -> Operators.assignmentShiftLeft,
    IASTBinaryExpression.op_shiftRightAssign -> Operators.assignmentArithmeticShiftRight,
    IASTBinaryExpression.op_binaryAndAssign  -> Operators.assignmentAnd,
    IASTBinaryExpression.op_binaryXorAssign  -> Operators.assignmentXor,
    IASTBinaryExpression.op_binaryOrAssign   -> Operators.assignmentOr,
    IASTBinaryExpression.op_equals           -> Operators.equals,
    IASTBinaryExpression.op_notequals        -> Operators.notEquals,
    IASTBinaryExpression.op_pmdot            -> Operators.indirectFieldAccess,
    IASTBinaryExpression.op_pmarrow          -> Operators.indirectFieldAccess,
    IASTBinaryExpression.op_max              -> Defines.OperatorMax,
    IASTBinaryExpression.op_min              -> Defines.OperatorMin,
    IASTBinaryExpression.op_ellipses         -> Defines.OperatorEllipses
  )

  private val UnaryOperatorMap: Map[Int, String] = Map(
    IASTUnaryExpression.op_prefixIncr       -> Operators.preIncrement,
    IASTUnaryExpression.op_prefixDecr       -> Operators.preDecrement,
    IASTUnaryExpression.op_plus             -> Operators.plus,
    IASTUnaryExpression.op_minus            -> Operators.minus,
    IASTUnaryExpression.op_star             -> Operators.indirection,
    IASTUnaryExpression.op_amper            -> Operators.addressOf,
    IASTUnaryExpression.op_tilde            -> Operators.not,
    IASTUnaryExpression.op_not              -> Operators.logicalNot,
    IASTUnaryExpression.op_sizeof           -> Operators.sizeOf,
    IASTUnaryExpression.op_postFixIncr      -> Operators.postIncrement,
    IASTUnaryExpression.op_postFixDecr      -> Operators.postDecrement,
    IASTUnaryExpression.op_typeid           -> Defines.OperatorTypeOf,
    IASTUnaryExpression.op_bracketedPrimary -> Defines.OperatorBracketedPrimary
  )

  protected def astForExpression(expression: IASTExpression): Ast = {
    val r = expression match {
      case lit: IASTLiteralExpression                                                => astForLiteral(lit)
      case un: IASTUnaryExpression if un.getOperator == IASTUnaryExpression.op_throw => astForThrowExpression(un)
      case un: IASTUnaryExpression                                                   => astForUnaryExpression(un)
      case bin: IASTBinaryExpression                                                 => astForBinaryExpression(bin)
      case exprList: IASTExpressionList                                              => astForExpressionList(exprList)
      case idExpr: IASTIdExpression                                                  => astForIdExpression(idExpr)
      case call: IASTFunctionCallExpression                                          => astForCallExpression(call)
      case typeId: IASTTypeIdExpression                                              => astForTypeIdExpression(typeId)
      case fieldRef: IASTFieldReference                                              => astForFieldReference(fieldRef)
      case expr: IASTConditionalExpression             => astForConditionalExpression(expr)
      case arr: IASTArraySubscriptExpression           => astForArrayIndexExpression(arr)
      case castExpression: IASTCastExpression          => astForCastExpression(castExpression)
      case newExpression: ICPPASTNewExpression         => astForNewExpression(newExpression)
      case delExpression: ICPPASTDeleteExpression      => astForDeleteExpression(delExpression)
      case typeIdInit: IASTTypeIdInitializerExpression => astForTypeIdInitExpression(typeIdInit)
      case c: ICPPASTSimpleTypeConstructorExpression   => astForConstructorExpression(c)
      case lambdaExpression: ICPPASTLambdaExpression   => astForLambdaExpression(lambdaExpression)
      case cExpr: IGNUASTCompoundStatementExpression   => astForCompoundStatementExpression(cExpr)
      case pExpr: ICPPASTPackExpansionExpression       => astForPackExpansionExpression(pExpr)
      case foldExpression: CPPASTFoldExpression        => astForFoldExpression(foldExpression)
      case _                                           => notHandledYet(expression)
    }
    asChildOfMacroCall(expression, r)
  }

  protected def astForStaticAssert(a: ICPPASTStaticAssertDeclaration): Ast = {
    val name    = "<operator>.staticAssert"
    val call    = callNode(a, code(a), name, name, DispatchTypes.STATIC_DISPATCH, None, Some(Defines.Any))
    val cond    = nullSafeAst(a.getCondition)
    val message = nullSafeAst(a.getMessage)
    callAst(call, List(cond, message))
  }

  private def astForBinaryExpression(bin: IASTBinaryExpression): Ast = {
    val op        = OperatorMap.getOrElse(bin.getOperator, Defines.OperatorUnknown)
    val callNode_ = callNode(bin, code(bin), op, op, DispatchTypes.STATIC_DISPATCH, None, Some(Defines.Any))
    val left      = nullSafeAst(bin.getOperand1)
    val right     = nullSafeAst(bin.getOperand2)
    callAst(callNode_, List(left, right))
  }

  private def astForExpressionList(exprList: IASTExpressionList): Ast = {
    val name = Defines.OperatorExpressionList
    val callNode_ =
      callNode(exprList, code(exprList), name, name, DispatchTypes.STATIC_DISPATCH, None, Some(Defines.Any))
    val childAsts = exprList.getExpressions.map(nullSafeAst)
    callAst(callNode_, childAsts.toIndexedSeq)
  }

  private def astForCppCallExpression(call: ICPPASTFunctionCallExpression): Ast = {
    val functionNameExpr = call.getFunctionNameExpression
    Try(functionNameExpr.getExpressionType).toOption match {
      case Some(_: IPointerType) => createPointerCallAst(call, cleanType(safeGetType(call.getExpressionType)))
      case Some(functionType: ICPPFunctionType) =>
        functionNameExpr match {
          case idExpr: CPPASTIdExpression if safeGetBinding(idExpr).exists(_.isInstanceOf[ICPPFunction]) =>
            val function  = idExpr.getName.getBinding.asInstanceOf[ICPPFunction]
            val name      = idExpr.getName.getLastName.toString
            val signature = if function.isExternC then "" else functionTypeToSignature(functionType)
            val fullName = if (function.isExternC) {
              StringUtils.normalizeSpace(name)
            } else {
              val fullNameNoSig = StringUtils.normalizeSpace(function.getQualifiedName.mkString("."))
              s"$fullNameNoSig:$signature"
            }
            val callCpgNode = callNode(
              call,
              code(call),
              name,
              fullName,
              DispatchTypes.STATIC_DISPATCH,
              Some(signature),
              Some(registerType(cleanType(safeGetType(call.getExpressionType))))
            )
            val args = call.getArguments.toList.map(a => astForNode(a))
            createCallAst(callCpgNode, args)
          case fieldRefExpr: ICPPASTFieldReference
              if safeGetBinding(fieldRefExpr.getFieldName).exists(_.isInstanceOf[ICPPMethod]) =>
            val instanceAst = astForExpression(fieldRefExpr.getFieldOwner)
            val args        = call.getArguments.toList.map(a => astForNode(a))

            // TODO This wont do if the name is a reference.
            val name      = fieldRefExpr.getFieldName.toString
            val signature = functionTypeToSignature(functionType)

            val classFullName = cleanType(safeGetType(fieldRefExpr.getFieldOwnerType))
            val fullName      = s"$classFullName.$name:$signature"

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
          case _ =>
            astForCppCallExpressionUntyped(call)
        }
      case Some(classType: ICPPClassType) if safeGetEvaluation(call).exists(_.isInstanceOf[EvalFunctionCall]) =>
        val evaluation   = call.getEvaluation.asInstanceOf[EvalFunctionCall]
        val functionType = Try(evaluation.getOverload.getType).toOption
        val signature    = functionType.map(functionTypeToSignature).getOrElse(X2CpgDefines.UnresolvedSignature)
        val name         = Defines.OperatorCall
        classType match {
          case _: CPPClosureType =>
            val fullName = s"$name:$signature"
            val callCpgNode = callNode(
              call,
              code(call),
              name,
              fullName,
              DispatchTypes.DYNAMIC_DISPATCH,
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
      case _ => astForCppCallExpressionUntyped(call)
    }
  }

  private def astForCppCallExpressionUntyped(call: ICPPASTFunctionCallExpression): Ast = {
    call.getFunctionNameExpression match {
      case fieldRefExpr: ICPPASTFieldReference =>
        val instanceAst = astForExpression(fieldRefExpr.getFieldOwner)
        val args        = call.getArguments.toList.map(a => astForNode(a))
        val name        = StringUtils.normalizeSpace(fieldRefExpr.getFieldName.toString)
        val signature   = X2CpgDefines.UnresolvedSignature
        val fullName    = s"${X2CpgDefines.UnresolvedNamespace}.$name:$signature(${args.size})"
        val callCpgNode =
          callNode(call, code(call), name, fullName, DispatchTypes.STATIC_DISPATCH, Some(signature), Some(Defines.Any))
        createCallAst(callCpgNode, args, base = Some(instanceAst), receiver = Some(instanceAst))
      case idExpr: CPPASTIdExpression =>
        val args      = call.getArguments.toList.map(a => astForNode(a))
        val name      = StringUtils.normalizeSpace(idExpr.getName.getLastName.toString)
        val signature = X2CpgDefines.UnresolvedSignature
        val fullName  = s"${X2CpgDefines.UnresolvedNamespace}.$name:$signature(${args.size})"
        val callCpgNode =
          callNode(call, code(call), name, fullName, DispatchTypes.STATIC_DISPATCH, Some(signature), Some(Defines.Any))
        createCallAst(callCpgNode, args)
      case otherExpr =>
        // This could either be a pointer or an operator() call we do not know at this point
        // but since it is CPP we opt for the latter.
        val args      = call.getArguments.toList.map(a => astForNode(a))
        val name      = Defines.OperatorCall
        val signature = X2CpgDefines.UnresolvedSignature
        val fullName  = s"${X2CpgDefines.UnresolvedNamespace}.$name:$signature(${args.size})"
        val callCpgNode =
          callNode(call, code(call), name, fullName, DispatchTypes.STATIC_DISPATCH, Some(signature), Some(Defines.Any))
        val instanceAst = astForExpression(otherExpr)
        createCallAst(callCpgNode, args, base = Some(instanceAst), receiver = Some(instanceAst))
    }
  }

  private def astForCCallExpression(call: CASTFunctionCallExpression): Ast = {
    val functionNameExpr = call.getFunctionNameExpression
    Try(functionNameExpr.getExpressionType).toOption match {
      case Some(_: CPointerType) =>
        createPointerCallAst(call, cleanType(safeGetType(call.getExpressionType)))
      case Some(_: CFunctionType) =>
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
    val dispatchType = DispatchTypes.STATIC_DISPATCH
    val callCpgNode =
      callNode(call, code(call), name, name, dispatchType, Some(""), Some(registerType(callTypeFullName)))
    val args = call.getArguments.toList.map(a => astForNode(a))
    createCallAst(callCpgNode, args)
  }

  private def createPointerCallAst(call: IASTFunctionCallExpression, callTypeFullName: String): Ast = {
    val functionNameExpr = call.getFunctionNameExpression
    val name             = Defines.OperatorPointerCall
    val dispatchType     = DispatchTypes.DYNAMIC_DISPATCH
    val callCpgNode =
      callNode(call, code(call), name, name, dispatchType, Some(""), Some(registerType(callTypeFullName)))
    val args        = call.getArguments.toList.map(a => astForNode(a))
    val receiverAst = astForExpression(functionNameExpr)
    createCallAst(callCpgNode, args, receiver = Some(receiverAst))
  }

  private def astForCCallExpressionUntyped(call: CASTFunctionCallExpression): Ast = {
    call.getFunctionNameExpression match {
      case idExpr: CASTIdExpression => createCFunctionCallAst(call, idExpr, Defines.Any)
      case _                        => createPointerCallAst(call, Defines.Any)
    }
  }

  private def astForCallExpression(call: IASTFunctionCallExpression): Ast = {
    call match {
      case cppCall: ICPPASTFunctionCallExpression => astForCppCallExpression(cppCall)
      case cCall: CASTFunctionCallExpression      => astForCCallExpression(cCall)
    }
  }

  private def astForThrowExpression(expression: IASTUnaryExpression): Ast = {
    val operand = nullSafeAst(expression.getOperand)
    Ast(controlStructureNode(expression, ControlStructureTypes.THROW, code(expression))).withChild(operand)
  }

  private def astForUnaryExpression(unary: IASTUnaryExpression): Ast = {
    val operatorMethod = UnaryOperatorMap.getOrElse(unary.getOperator, Defines.OperatorUnknown)
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
        Some(Defines.Any)
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
            Some(Defines.Any)
          )
        val arg = astForNode(typeId.getTypeId.getDeclSpecifier)
        callAst(call, List(arg))
      case _ => notHandledYet(typeId)
    }
  }

  private def astForConditionalExpression(expr: IASTConditionalExpression): Ast = {
    val name = Operators.conditional
    val call = callNode(expr, code(expr), name, name, DispatchTypes.STATIC_DISPATCH, None, Some(Defines.Any))

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
        Some(Defines.Any)
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
        Some(Defines.Any)
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
    val cpgNewExpression =
      callNode(newExpression, code(newExpression), name, name, DispatchTypes.STATIC_DISPATCH, None, Some(Defines.Any))

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
      callNode(delExpression, code(delExpression), name, name, DispatchTypes.STATIC_DISPATCH, None, Some(Defines.Any))
    val arg = astForExpression(delExpression.getOperand)
    callAst(cpgDeleteNode, List(arg))
  }

  private def astForTypeIdInitExpression(typeIdInit: IASTTypeIdInitializerExpression): Ast = {
    val name = Operators.cast
    val cpgCastExpression =
      callNode(typeIdInit, code(typeIdInit), name, name, DispatchTypes.STATIC_DISPATCH, None, Some(Defines.Any))
    val typeAst = unknownNode(typeIdInit.getTypeId, code(typeIdInit.getTypeId))
    val expr    = astForNode(typeIdInit.getInitializer)
    callAst(cpgCastExpression, List(Ast(typeAst), expr))
  }

  private def astForConstructorExpression(c: ICPPASTSimpleTypeConstructorExpression): Ast = {
    val name = c.getDeclSpecifier.toString
    c.getInitializer match {
      case l: ICPPASTInitializerList if l.getClauses.forall(_.isInstanceOf[ICPPASTDesignatedInitializer]) =>
        val node = blockNode(c)
        scope.pushNewBlockScope(node)

        val inits = l.getClauses.collect { case i: ICPPASTDesignatedInitializer => i }.toSeq
        val calls = inits.flatMap { init =>
          val designatorIds = init.getDesignators.collect { case d: ICPPASTFieldDesignator =>
            val name = code(d.getName)
            fieldIdentifierNode(d, name, name)
          }
          designatorIds.map { memberId =>
            val rhsAst = astForNode(init.getOperand)
            val specifierId =
              identifierNode(c.getDeclSpecifier, name, name, registerType(cleanType(typeFor(c.getDeclSpecifier))))
            val op         = Operators.fieldAccess
            val accessCode = s"$name.${memberId.code}"
            val ma         = callNode(init, accessCode, op, op, DispatchTypes.STATIC_DISPATCH, None, Some(Defines.Any))
            val maAst      = callAst(ma, List(Ast(specifierId), Ast(memberId)))
            val assignmentCallNode =
              callNode(
                c,
                s"$accessCode = ${code(init.getOperand)}",
                Operators.assignment,
                Operators.assignment,
                DispatchTypes.STATIC_DISPATCH,
                None,
                Some(Defines.Any)
              )
            callAst(assignmentCallNode, List(maAst, rhsAst))
          }
        }

        scope.popScope()
        blockAst(node, calls.toList)
      case other =>
        val callNode_ = callNode(c, code(c), name, name, DispatchTypes.STATIC_DISPATCH, None, Some(Defines.Any))
        val arg       = astForNode(other)
        callAst(callNode_, List(arg))
    }
  }

  private def astForCompoundStatementExpression(compoundExpression: IGNUASTCompoundStatementExpression): Ast =
    nullSafeAst(compoundExpression.getCompoundStatement).headOption.getOrElse(Ast())

  private def astForPackExpansionExpression(packExpansionExpression: ICPPASTPackExpansionExpression): Ast =
    astForExpression(packExpansionExpression.getPattern)

  private def astForFoldExpression(foldExpression: CPPASTFoldExpression): Ast = {
    def valueFromField[T](obj: Any, fieldName: String): Option[T] = {
      // we need this hack because fields are all private at CPPASTExpression
      Try {
        val field = obj.getClass.getDeclaredField(fieldName)
        field.setAccessible(true)
        field.get(obj).asInstanceOf[T]
      }.toOption
    }

    val op  = "<operator>.fold"
    val tpe = typeFor(foldExpression)
    val callNode_ =
      callNode(foldExpression, code(foldExpression), op, op, DispatchTypes.STATIC_DISPATCH, None, Some(tpe))

    val left  = valueFromField[ICPPASTExpression](foldExpression, "fLhs").map(nullSafeAst).getOrElse(Ast())
    val right = valueFromField[ICPPASTExpression](foldExpression, "fRhs").map(nullSafeAst).getOrElse(Ast())
    callAst(callNode_, List(left, right))
  }

  private def astForIdExpression(idExpression: IASTIdExpression): Ast = idExpression.getName match {
    case name: CPPASTQualifiedName                                => astForQualifiedName(name)
    case name: ICPPASTName if name.getRawSignature == "constinit" => Ast()
    case _                                                        => astForIdentifier(idExpression)
  }

}
