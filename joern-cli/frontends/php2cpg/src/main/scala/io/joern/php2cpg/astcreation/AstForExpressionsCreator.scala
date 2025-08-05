package io.joern.php2cpg.astcreation

import io.joern.php2cpg.astcreation.AstCreator.{NameConstants, TypeConstants, operatorSymbols}
import io.joern.php2cpg.datastructures.ArrayIndexTracker
import io.joern.php2cpg.parser.Domain.*
import io.joern.php2cpg.utils.BlockScope
import io.joern.x2cpg.utils.AstPropertiesUtil.RootProperties
import io.joern.x2cpg.{Ast, Defines, ValidationMode}
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.codepropertygraph.generated.{ControlStructureTypes, DispatchTypes, Operators, PropertyNames}
import io.shiftleft.semanticcpg.language.types.structure.NamespaceTraversal

import scala.collection.mutable

trait AstForExpressionsCreator(implicit withSchemaValidation: ValidationMode) { this: AstCreator =>

  protected def astForExpr(expr: PhpExpr): Ast = {
    expr match {
      case funcCallExpr: PhpCallExpr                   => astForCall(funcCallExpr)
      case variableExpr: PhpVariable                   => astForVariableExpr(variableExpr)
      case nameExpr: PhpNameExpr                       => astForNameExpr(nameExpr)
      case assignExpr: PhpAssignment                   => astForAssignment(assignExpr)
      case scalarExpr: PhpScalar                       => astForScalar(scalarExpr)
      case binaryOp: PhpBinaryOp                       => astForBinOp(binaryOp)
      case unaryOp: PhpUnaryOp                         => astForUnaryOp(unaryOp)
      case castExpr: PhpCast                           => astForCastExpr(castExpr)
      case isSetExpr: PhpIsset                         => astForIsSetExpr(isSetExpr)
      case printExpr: PhpPrint                         => astForPrintExpr(printExpr)
      case ternaryOp: PhpTernaryOp                     => astForTernaryOp(ternaryOp)
      case throwExpr: PhpThrowExpr                     => astForThrow(throwExpr)
      case cloneExpr: PhpCloneExpr                     => astForClone(cloneExpr)
      case emptyExpr: PhpEmptyExpr                     => astForEmpty(emptyExpr)
      case evalExpr: PhpEvalExpr                       => astForEval(evalExpr)
      case exitExpr: PhpExitExpr                       => astForExit(exitExpr)
      case arrayExpr: PhpArrayExpr                     => astForArrayExpr(arrayExpr)
      case newExpr: PhpNewExpr                         => astForNewExpr(newExpr)
      case matchExpr: PhpMatchExpr                     => astForMatchExpr(matchExpr)
      case yieldExpr: PhpYieldExpr                     => astForYieldExpr(yieldExpr)
      case closure: PhpClosureExpr                     => astForClosureExpr(closure)
      case yieldFromExpr: PhpYieldFromExpr             => astForYieldFromExpr(yieldFromExpr)
      case classConstFetchExpr: PhpClassConstFetchExpr => astForClassConstFetchExpr(classConstFetchExpr)
      case constFetchExpr: PhpConstFetchExpr           => astForConstFetchExpr(constFetchExpr)
      case arrayDimFetchExpr: PhpArrayDimFetchExpr     => astForArrayDimFetchExpr(arrayDimFetchExpr)
      case errorSuppressExpr: PhpErrorSuppressExpr     => astForErrorSuppressExpr(errorSuppressExpr)
      case instanceOfExpr: PhpInstanceOfExpr           => astForInstanceOfExpr(instanceOfExpr)
      case propertyFetchExpr: PhpPropertyFetchExpr     => astForPropertyFetchExpr(propertyFetchExpr)
      case includeExpr: PhpIncludeExpr                 => astForIncludeExpr(includeExpr)
      case shellExecExpr: PhpShellExecExpr             => astForShellExecExpr(shellExecExpr)
      case null =>
        logger.warn("expr was null")
        ???
      case other => throw new NotImplementedError(s"unexpected expression '$other' of type ${other.getClass}")
    }
  }

  private def astForCall(call: PhpCallExpr): Ast = {
    val nameAst   = Option.unless(call.methodName.isInstanceOf[PhpNameExpr])(astForExpr(call.methodName))
    val name      = getCallName(call, nameAst)
    val arguments = call.args.map(astForCallArg)

    /*
     * A receiver only makes sense if one can track the receiver back to some sort of runtime type information. In the
     * case of a normal top-level call like foo() that is not possible. There is no corresponding foo identifier which
     * one could assign another function to. In terms of lambdas, they will be assigned to a variable which always
     * begins with `$`.
     *
     * The only uncertainty in such a call is whether this is a foo in the global namespace or from the namespace
     * surrounding the call-site. But this we cannot decide without knowing which PHP files are loaded. For built-in
     * calls, we refer to the `builtin_functions.txt` list under `resources`, which is tied to the global namespace,
     * e.g., if `foo` is global, the full name will be `foo`.
     *
     * If we import some symbol from a namespace, e.g., `A\B\foo`, is this a constant, field, or class? PHP makes this
     * clear with `use function x` or `use constant y`. However, in the case of `require` this is still ambiguous.
     * If at any point a symbol's full name cannot be resolved, which is often in the case of chained calls of external
     * API's or dynamic calls, the method full name is prefixed with <unresolvedNamespace>.
     *
     * This is why we use pre-parse summary idea where we check which functions exist in which namespaces and make a
     * lookup in it to figure out whether a foo call-site needs to have an imported/external namespace or one from the
     * current namespace. The `scope` object then delegates what has been defined or imported to determine calls in
     * scope.
     */
    call.target match {
      case None if scope.isTopLevel || isBuiltinFunc(name) => astForStaticCall(call, name, arguments)
      case _ if call.isStatic                              => astForStaticCall(call, name, arguments)
      case maybeTarget                                     => astForDynamicCall(call, name, arguments, maybeTarget)
    }
  }

  private def astForDynamicCall(
    call: PhpCallExpr,
    name: String,
    arguments: Seq[Ast],
    maybeTarget: Option[PhpExpr]
  ): Ast = {
    val argsCode   = getArgsCode(call, arguments)
    val targetAst  = maybeTarget.map(astForExpr)
    val codePrefix = targetAst.map(codeForMethodCall(call, _, name)).getOrElse(name)
    val code       = s"$codePrefix($argsCode)"

    val dispatchType = DispatchTypes.DYNAMIC_DISPATCH

    val fullName = getMfn(call, name)

    val callRoot = callNode(call, code, name, fullName, dispatchType, None, Some(Defines.Any))

    val receiverAst = targetAst match {
      case None if !scope.isTopLevel =>
        // if dynamic call is under some type decl, $this is the receiver
        call.target.map(thisIdentifier).orElse(Option(thisIdentifier(call))).map(Ast(_))
      case None =>
        // if dynamic call is on "global" level, the variable itself is the receiver
        Option(astForExpr(call.methodName))
      case someTarget => someTarget
    }

    callAst(callRoot, arguments, receiverAst)
  }

  private def astForStaticCall(call: PhpCallExpr, name: String, arguments: Seq[Ast]): Ast = {
    val argsCode   = getArgsCode(call, arguments)
    val codePrefix = codeForStaticMethodCall(call, name)
    val code       = s"$codePrefix($argsCode)"

    val dispatchType = DispatchTypes.STATIC_DISPATCH

    // In some cases, the parser is able to obtain the fully qualified name from imports
    val fullName = call.methodName match {
      case name: PhpNameExpr if name.name.contains("\\") => name.name
      case _                                             => getMfn(call, name)
    }

    val callRoot = callNode(call, code, name, fullName, dispatchType, None, Some(Defines.Any))

    callAst(callRoot, arguments)
  }

  protected def simpleAssignAst(origin: PhpNode, target: Ast, source: Ast): Ast = {
    val code     = s"${target.rootCodeOrEmpty} = ${source.rootCodeOrEmpty}"
    val callNode = operatorCallNode(origin, code, Operators.assignment, None)
    callAst(callNode, target :: source :: Nil)
  }

  protected def astForAssignment(assignment: PhpAssignment): Ast = {
    assignment.target match {
      case arrayDimFetch: PhpArrayDimFetchExpr if arrayDimFetch.dimension.isEmpty =>
        // Rewrite `$xs[] = <value_expr>` as `array_push($xs, <value_expr>)` to simplify finding dataflows.
        astForEmptyArrayDimAssign(assignment, arrayDimFetch)
      case arrayExpr: (PhpArrayExpr | PhpListExpr) =>
        astForArrayUnpack(assignment, arrayExpr, astForExpr(assignment.source))
      case _ =>
        val operatorName = assignment.assignOp

        val targetAst = astForExpr(assignment.target)
        val sourceAst = astForExpr(assignment.source)

        // TODO Handle ref assigns properly (if needed).
        val refSymbol = if (assignment.isRefAssign) "&" else ""
        val symbol    = operatorSymbols.getOrElse(assignment.assignOp, assignment.assignOp)
        val code      = s"${targetAst.rootCodeOrEmpty} $symbol $refSymbol${sourceAst.rootCodeOrEmpty}"

        val callNode = operatorCallNode(assignment, code, operatorName, None)
        callAst(callNode, List(targetAst, sourceAst))
    }
  }

  /** This is used to rewrite the short form $xs[] = <value_expr> as array_push($xs, <value_expr>) to avoid having to
    * handle the empty array access operator as a special case in the dataflow engine.
    *
    * This representation is technically wrong in the case where the shorthand is used to initialise a new array (since
    * PHP expects the first argument to array_push to be an existing array). This shouldn't affect dataflow, however.
    */
  private def astForEmptyArrayDimAssign(assignment: PhpAssignment, arrayDimFetch: PhpArrayDimFetchExpr): Ast = {
    val attrs         = assignment.attributes
    val arrayPushArgs = List(arrayDimFetch.variable, assignment.source).map(PhpArg(_))
    val arrayPushCall = PhpCallExpr(
      target = None,
      methodName = PhpNameExpr("array_push", attrs),
      args = arrayPushArgs,
      isNullSafe = false,
      isStatic = true,
      attributes = attrs
    )
    val arrayPushAst = astForCall(arrayPushCall)
    arrayPushAst.root.collect { case astRoot: NewCall =>
      val args =
        arrayPushAst.argEdges
          .filter(_.src == astRoot)
          .map(_.dst)
          .collect { case arg: ExpressionNew => arg }
          .sortBy(_.argumentIndex)

      if (args.size != 2) {
        val position = s"${line(assignment).getOrElse("")}:$relativeFileName"
        logger.warn(s"Expected 2 call args for emptyArrayDimAssign. Not resetting code: $position")
      } else {
        val codeOverride = s"${args.head.code}[] = ${args.last.code}"
        astRoot.code(codeOverride)
      }
    }
    arrayPushAst
  }

  protected def astForMemberAssignment(
    originNode: PhpNode,
    memberNode: NewMember,
    valueExpr: PhpExpr,
    isField: Boolean
  ): Ast = {
    val targetAst = if (isField) {
      val code            = s"$$this$InstanceMethodDelimiter${memberNode.name}"
      val fieldAccessNode = operatorCallNode(originNode, code, Operators.fieldAccess, None)
      val identifier      = thisIdentifier(originNode)
      val thisParam       = scope.lookupVariable(NameConstants.This)
      val fieldIdentifier = fieldIdentifierNode(originNode, memberNode.name, memberNode.name)
      callAst(fieldAccessNode, List(identifier, fieldIdentifier).map(Ast(_))).withRefEdges(identifier, thisParam.toList)
    } else {
      val selfIdentifier = {
        val name = NameConstants.Self
        val typ  = scope.getEnclosingTypeDeclTypeName
        identifierNode(originNode, name, name, typ.getOrElse(Defines.Any), typ.toList)
      }

      val selfLocal = handleVariableOccurrence(originNode, selfIdentifier.name)

      val fieldIdentifier = fieldIdentifierNode(originNode, memberNode.name, memberNode.name)
      val code = s"${NameConstants.Self}$StaticMethodDelimiter${memberNode.code.replaceAll("(static|case|const) ", "")}"
      val fieldAccessNode = operatorCallNode(originNode, code, Operators.fieldAccess, None)
      val selfIdentAst    = astForIdentifierWithLocalRef(selfIdentifier, selfLocal)
      val callArgs        = List(selfIdentAst, Ast(fieldIdentifier))
      callAst(fieldAccessNode, callArgs)
    }

    val value = astForExpr(valueExpr)

    val assignmentCode = s"${targetAst.rootCodeOrEmpty} = ${value.rootCodeOrEmpty}"
    val callNode       = operatorCallNode(originNode, assignmentCode, Operators.assignment, None)

    callAst(callNode, List(targetAst, value))
  }

  /** Lower the array/list unpack. For example `[$a, $b] = $arr;` will be lowered to `$a = $arr[0]; $b = $arr[1];`
    */
  protected def astForArrayUnpack(assignment: PhpNode, target: PhpArrayExpr | PhpListExpr, sourceAst: Ast): Ast = {
    val loweredAssignNodes = mutable.ListBuffer.empty[Ast]

    // create a Identifier ast for given name
    def createIdentifier(name: String): Ast = Ast(identifierNode(assignment, name, s"$$$name", Defines.Any))

    def createIndexAccessChain(
      targetAst: Ast,
      sourceAst: Ast,
      idxTracker: ArrayIndexTracker,
      item: PhpArrayItem
    ): Ast = {
      // copy from `assignForArrayItem` to handle the case where key exists, such as `list("id" => $a, "name" => $b) = $arr;`
      val dimension = item.key match {
        case Some(key: PhpSimpleScalar) => dimensionFromSimpleScalar(key, idxTracker)
        case Some(key)                  => key
        case None                       => PhpInt(idxTracker.next, item.attributes)
      }
      val dimensionAst    = astForExpr(dimension)
      val indexAccessCode = s"${sourceAst.rootCodeOrEmpty}[${dimensionAst.rootCodeOrEmpty}]"
      // <operator>.indexAccess(sourceAst, index)
      val indexAccessNode =
        callAst(operatorCallNode(item, indexAccessCode, Operators.indexAccess, None), sourceAst :: dimensionAst :: Nil)
      val assignCode = s"${targetAst.rootCodeOrEmpty} = $indexAccessCode"
      val assignNode = operatorCallNode(item, assignCode, Operators.assignment, None)
      // targetAst = <operator>.indexAccess(sourceAst, index)
      callAst(assignNode, targetAst :: indexAccessNode :: Nil)
    }

    // Take `[[$a, $b], $c] = $arr;` as an example
    def handleUnpackLowering(
      target: PhpArrayExpr | PhpListExpr,
      itemsOf: PhpArrayExpr | PhpListExpr => List[Option[PhpArrayItem]],
      sourceAst: Ast
    ): Unit = {
      val idxTracker = new ArrayIndexTracker

      // create an alias identifier of $arr
      val sourceAliasName       = this.scope.getNewVarTmp()
      val sourceAliasIdentifier = createIdentifier(sourceAliasName)
      val assignCode            = s"${sourceAliasIdentifier.rootCodeOrEmpty} = ${sourceAst.rootCodeOrEmpty}"
      val assignNode            = operatorCallNode(assignment, assignCode, Operators.assignment, None)
      loweredAssignNodes += callAst(assignNode, sourceAliasIdentifier :: sourceAst :: Nil)

      itemsOf(target).foreach {
        case Some(item) =>
          item.value match {
            case nested: (PhpArrayExpr | PhpListExpr) => // item is [$a, $b]
              // create tmp variable for [$a, $b] to receive the result of <operator>.indexAccess($arr, 0)
              val tmpIdentifierName = this.scope.getNewVarTmp()
              // tmpVar = <operator>.indexAccess($arr, 0)
              val targetAssignNode =
                createIndexAccessChain(
                  createIdentifier(tmpIdentifierName),
                  createIdentifier(sourceAliasName),
                  idxTracker,
                  item
                )
              loweredAssignNodes += targetAssignNode
              handleUnpackLowering(nested, itemsOf, createIdentifier(tmpIdentifierName))
            case phpVar: PhpVariable => // item is $c
              val identifier = astForExpr(phpVar)
              // $c = <operator>.indexAccess($arr, 1)
              val targetAssignNode =
                createIndexAccessChain(identifier, createIdentifier(sourceAliasName), idxTracker, item)
              loweredAssignNodes += targetAssignNode
            case _ =>
              // unknown case
              idxTracker.next
          }
        case None =>
          idxTracker.next
      }
    }

    val itemsOf = (exp: PhpArrayExpr | PhpListExpr) =>
      exp match {
        case x: PhpArrayExpr => x.items
        case x: PhpListExpr  => x.items
      }
    handleUnpackLowering(target, itemsOf, sourceAst)
    Ast(blockNode(assignment))
      .withChildren(loweredAssignNodes.toList)
  }

  private def astForEncapsed(encapsed: PhpEncapsed): Ast = {
    val args = encapsed.parts.map(astForExpr)
    val code = args.map(_.rootCodeOrEmpty).mkString(" . ")

    args match {
      case singleArg :: Nil => singleArg
      case _ =>
        val callNode = operatorCallNode(encapsed, code, PhpOperators.encaps, Some(TypeConstants.String))
        callAst(callNode, args)
    }
  }

  private def astForScalar(scalar: PhpScalar): Ast = {
    scalar match {
      case encapsed: PhpEncapsed         => astForEncapsed(encapsed)
      case simpleScalar: PhpSimpleScalar => Ast(literalNode(scalar, simpleScalar.value, simpleScalar.typeFullName))
      case null =>
        logger.warn("scalar was null")
        ???
    }
  }

  private def astForBinOp(binOp: PhpBinaryOp): Ast = {
    val leftAst  = astForExpr(binOp.left)
    val rightAst = astForExpr(binOp.right)

    val symbol = operatorSymbols.getOrElse(binOp.operator, binOp.operator)
    val code   = s"${leftAst.rootCodeOrEmpty} $symbol ${rightAst.rootCodeOrEmpty}"

    val callNode = operatorCallNode(binOp, code, binOp.operator, None)

    callAst(callNode, List(leftAst, rightAst))
  }

  private def isPostfixOperator(operator: String): Boolean = {
    Set(Operators.postDecrement, Operators.postIncrement).contains(operator)
  }

  private def astForUnaryOp(unaryOp: PhpUnaryOp): Ast = {
    val exprAst = astForExpr(unaryOp.expr)

    val symbol = operatorSymbols.getOrElse(unaryOp.operator, unaryOp.operator)
    val code =
      if (isPostfixOperator(unaryOp.operator))
        s"${exprAst.rootCodeOrEmpty}$symbol"
      else
        s"$symbol${exprAst.rootCodeOrEmpty}"

    val callNode = operatorCallNode(unaryOp, code, unaryOp.operator, None)

    callAst(callNode, exprAst :: Nil)
  }

  private def astForCastExpr(castExpr: PhpCast): Ast = {
    val typeFullName = castExpr.typ
    val typ          = typeRefNode(castExpr, typeFullName, typeFullName)

    val expr    = astForExpr(castExpr.expr)
    val codeStr = s"($typeFullName) ${expr.rootCodeOrEmpty}"

    val callNode = operatorCallNode(castExpr, codeStr, Operators.cast, Some(typeFullName))

    callAst(callNode, Ast(typ) :: expr :: Nil)
  }

  private def astForIsSetExpr(isSetExpr: PhpIsset): Ast = {
    val name = PhpOperators.issetFunc
    val args = isSetExpr.vars.map(astForExpr)
    val code = s"$name(${args.map(_.rootCodeOrEmpty).mkString(",")})"

    val callNode =
      operatorCallNode(isSetExpr, code, name, Some(TypeConstants.Bool))
        .methodFullName(PhpOperators.issetFunc)

    callAst(callNode, args)
  }

  private def astForPrintExpr(printExpr: PhpPrint): Ast = {
    val name = PhpOperators.printFunc
    val arg  = astForExpr(printExpr.expr)
    val code = s"$name(${arg.rootCodeOrEmpty})"

    val callNode =
      operatorCallNode(printExpr, code, name, typeFullName = Some(TypeConstants.Int))
        .methodFullName(PhpOperators.printFunc)

    callAst(callNode, arg :: Nil)
  }

  private def astForTernaryOp(ternaryOp: PhpTernaryOp): Ast = {
    val conditionAst = astForExpr(ternaryOp.condition)
    val maybeThenAst = ternaryOp.thenExpr.map(astForExpr)
    val elseAst      = astForExpr(ternaryOp.elseExpr)

    val operatorName = if (maybeThenAst.isDefined) Operators.conditional else PhpOperators.elvisOp
    val code = maybeThenAst match {
      case Some(thenAst) => s"${conditionAst.rootCodeOrEmpty} ? ${thenAst.rootCodeOrEmpty} : ${elseAst.rootCodeOrEmpty}"
      case None          => s"${conditionAst.rootCodeOrEmpty} ?: ${elseAst.rootCodeOrEmpty}"
    }

    val callNode = operatorCallNode(ternaryOp, code, operatorName, None)

    val args = List(Option(conditionAst), maybeThenAst, Option(elseAst)).flatten
    callAst(callNode, args)
  }

  protected def astForCallArg(arg: PhpArgument): Ast = {
    arg match {
      case PhpArg(expr, _, _, _, _) =>
        astForExpr(expr)

      case _: PhpVariadicPlaceholder =>
        val identifier = identifierNode(arg, "...", "...", TypeConstants.VariadicPlaceholder)
        val local      = handleVariableOccurrence(arg, "...")
        astForIdentifierWithLocalRef(identifier, local)
    }
  }

  private def astForVariableExpr(variable: PhpVariable): Ast = {
    // TODO Need to figure out variable variables. Maybe represent as some kind of call?
    val valueAst = astForExpr(variable.value)

    valueAst.root.collect { case root: ExpressionNew =>
      root.code = s"$$${root.code}"
    }

    valueAst.root.collect { case root: NewIdentifier =>
      root.lineNumber = line(variable)
    }

    valueAst
  }

  private def astForNameExpr(expr: PhpNameExpr): Ast = {
    val identifier = identifierNode(expr, expr.name, expr.name, Defines.Any)

    val declaringNode = handleVariableOccurrence(expr, identifier.name)

    Ast(identifier).withRefEdges(identifier, List(declaringNode))
  }

  protected def stmtBodyBlockAst(stmt: PhpStmtWithBody): Ast = {
    val bodyBlock    = blockNode(stmt)
    val bodyStmtAsts = stmt.stmts.flatMap(astsForStmt)
    Ast(bodyBlock).withChildren(bodyStmtAsts)
  }

  protected def astForKeyValPair(origin: PhpNode, key: PhpExpr, value: PhpExpr): Ast = {
    val keyAst   = astForExpr(key)
    val valueAst = astForExpr(value)

    val code     = s"${keyAst.rootCodeOrEmpty} => ${valueAst.rootCodeOrEmpty}"
    val callNode = operatorCallNode(origin, code, PhpOperators.doubleArrow, None)
    callAst(callNode, keyAst :: valueAst :: Nil)
  }

  private def astForErrorSuppressExpr(expr: PhpErrorSuppressExpr): Ast = {
    val childAst = astForExpr(expr.expr)

    val code         = s"@${childAst.rootCodeOrEmpty}"
    val suppressNode = operatorCallNode(expr, code, PhpOperators.errorSuppress, None)
    childAst.rootType.foreach(typ => suppressNode.typeFullName(typ))

    callAst(suppressNode, childAst :: Nil)
  }

  private def astForInstanceOfExpr(expr: PhpInstanceOfExpr): Ast = {
    val exprAst  = astForExpr(expr.expr)
    val classAst = astForExpr(expr.className)

    val code           = s"${exprAst.rootCodeOrEmpty} instanceof ${classAst.rootCodeOrEmpty}"
    val instanceOfNode = operatorCallNode(expr, code, Operators.instanceOf, Some(TypeConstants.Bool))

    callAst(instanceOfNode, exprAst :: classAst :: Nil)
  }

  private def astForPropertyFetchExpr(expr: PhpPropertyFetchExpr): Ast = {

    def fieldNodeAndName(nameExpr: PhpExpr): (PhpExpr, Option[String]) = nameExpr match {
      case name: PhpNameExpr => (name, Option(name.name))
      case variable: PhpVariable =>
        val (expr, maybeName) = fieldNodeAndName(variable.value)
        (expr, maybeName.map(name => s"$$$name"))
      case other => (other, None)
    }

    val (fieldNode, fieldName) = fieldNodeAndName(expr.name)

    val fieldAst = fieldName match {
      case None =>
        astForExpr(fieldNode)
      case Some(name) => Ast(fieldIdentifierNode(expr, name.stripPrefix("$"), name))
    }

    val operatorName = fieldName match {
      case Some(_) => Operators.fieldAccess
      case None    => Operators.indexAccess
    }

    val accessSymbol =
      if (expr.isStatic) StaticMethodDelimiter
      else if (expr.isNullsafe) s"?$InstanceMethodDelimiter"
      else InstanceMethodDelimiter

    val targetAst  = astForExpr(expr.expr)
    val targetCode = targetAst.rootCodeOrEmpty
    val fieldAstCode = fieldName match {
      case Some(_) => fieldAst.rootCodeOrEmpty
      case None    => s"{${fieldAst.rootCodeOrEmpty}}"
    }
    val code            = s"$targetCode$accessSymbol$fieldAstCode"
    val fieldAccessNode = operatorCallNode(expr, code, operatorName, None)
    callAst(fieldAccessNode, Seq(targetAst, fieldAst))
  }

  private def astForIncludeExpr(expr: PhpIncludeExpr): Ast = {
    val exprAst  = astForExpr(expr.expr)
    val code     = s"${expr.includeType} ${exprAst.rootCodeOrEmpty}"
    val callNode = operatorCallNode(expr, code, expr.includeType, None)

    callAst(callNode, exprAst :: Nil)
  }

  private def astForShellExecExpr(expr: PhpShellExecExpr): Ast = {
    val args = astForEncapsed(expr.parts)
    val code = "`" + args.rootCodeOrEmpty + "`"

    val callNode = operatorCallNode(expr, code, PhpOperators.shellExec, None)

    callAst(callNode, args :: Nil)
  }

  private def astForClone(expr: PhpCloneExpr): Ast = {
    val name    = PhpOperators.cloneFunc
    val argAst  = astForExpr(expr.expr)
    val argType = argAst.rootType.orElse(Some(Defines.Any))
    val code    = s"$name ${argAst.rootCodeOrEmpty}"

    val callNode = operatorCallNode(expr, code, name, argType)
      .methodFullName(PhpOperators.cloneFunc)

    callAst(callNode, argAst :: Nil)
  }

  private def astForEmpty(expr: PhpEmptyExpr): Ast = {
    val name   = PhpOperators.emptyFunc
    val argAst = astForExpr(expr.expr)
    val code   = s"$name(${argAst.rootCodeOrEmpty})"

    val callNode =
      operatorCallNode(expr, code, name, Some(TypeConstants.Bool))
        .methodFullName(PhpOperators.emptyFunc)

    callAst(callNode, argAst :: Nil)
  }

  private def astForEval(expr: PhpEvalExpr): Ast = {
    val name   = PhpOperators.evalFunc
    val argAst = astForExpr(expr.expr)
    val code   = s"$name(${argAst.rootCodeOrEmpty})"

    val callNode =
      operatorCallNode(expr, code, name, Some(TypeConstants.Bool))
        .methodFullName(PhpOperators.evalFunc)

    callAst(callNode, argAst :: Nil)
  }

  private def astForExit(expr: PhpExitExpr): Ast = {
    val name = PhpOperators.exitFunc
    val args = expr.expr.map(astForExpr)
    val code = s"$name(${args.map(_.rootCodeOrEmpty).getOrElse("")})"

    val callNode = operatorCallNode(expr, code, name, Some(TypeConstants.Void))
      .methodFullName(PhpOperators.exitFunc)

    callAst(callNode, args.toList)
  }

  private def astForArrayExpr(expr: PhpArrayExpr): Ast = {
    val idxTracker = new ArrayIndexTracker

    val tmpName = this.scope.getNewVarTmp()

    val local = handleVariableOccurrence(expr, tmpName)

    def newTmpIdentifier: Ast = {
      val identifier = identifierNode(expr, tmpName, s"$$$tmpName", TypeConstants.Array)
      astForIdentifierWithLocalRef(identifier, local)
    }

    val tmpIdentifierAssignNode = {
      // use array() function to create an empty array. see https://www.php.net/manual/zh/function.array.php
      val initArrayNode = callNode(
        expr,
        "array()",
        "array",
        "array",
        DispatchTypes.STATIC_DISPATCH,
        Some("array()"),
        Some(TypeConstants.Array)
      )
      val initArrayCallAst = callAst(initArrayNode)

      val assignCode = s"$$$tmpName = ${initArrayCallAst.rootCodeOrEmpty}"
      val assignNode = operatorCallNode(expr, assignCode, Operators.assignment, None)
      callAst(assignNode, newTmpIdentifier :: initArrayCallAst :: Nil)
    }

    val itemAssignments = expr.items.flatMap {
      case Some(item) => Option(assignForArrayItem(item, tmpName, idxTracker))
      case None =>
        idxTracker.next // Skip an index
        None
    }
    val arrayBlock = blockNode(expr)

    Ast(arrayBlock)
      .withChild(tmpIdentifierAssignNode)
      .withChildren(itemAssignments)
      .withChild(newTmpIdentifier)
  }

  private def assignForArrayItem(item: PhpArrayItem, name: String, idxTracker: ArrayIndexTracker): Ast = {
    // It's perhaps a bit clumsy to reconstruct PhpExpr nodes here, but reuse astForArrayDimExpr for consistency
    val variable = PhpVariable(PhpNameExpr(name, item.attributes), item.attributes)

    val dimension = item.key match {
      case Some(key: PhpSimpleScalar) => dimensionFromSimpleScalar(key, idxTracker)
      case Some(key)                  => key
      case None                       => PhpInt(idxTracker.next, item.attributes)
    }

    val dimFetchNode = PhpArrayDimFetchExpr(variable, Option(dimension), item.attributes)
    val dimFetchAst  = astForArrayDimFetchExpr(dimFetchNode)

    val valueAst = astForArrayItemValue(item)

    val assignCode = s"${dimFetchAst.rootCodeOrEmpty} = ${valueAst.rootCodeOrEmpty}"

    val assignNode = operatorCallNode(item, assignCode, Operators.assignment, None)

    callAst(assignNode, dimFetchAst :: valueAst :: Nil)
  }

  private def astForArrayItemValue(item: PhpArrayItem): Ast = {
    val exprAst   = astForExpr(item.value)
    val valueCode = exprAst.rootCodeOrEmpty

    if (item.byRef) {
      val parentCall = operatorCallNode(item, s"&$valueCode", Operators.addressOf, None)
      callAst(parentCall, exprAst :: Nil)
    } else if (item.unpack) {
      val parentCall = operatorCallNode(item, s"...$valueCode", PhpOperators.unpack, None)
      callAst(parentCall, exprAst :: Nil)
    } else {
      exprAst
    }
  }

  private def astForArrayDimFetchExpr(expr: PhpArrayDimFetchExpr): Ast = {
    val variableAst  = astForExpr(expr.variable)
    val variableCode = variableAst.rootCodeOrEmpty

    expr.dimension match {
      case Some(dimension) =>
        val dimensionAst = astForExpr(dimension)
        val code         = s"$variableCode[${dimensionAst.rootCodeOrEmpty}]"
        val accessNode   = operatorCallNode(expr, code, Operators.indexAccess, None)
        callAst(accessNode, variableAst :: dimensionAst :: Nil)

      case None =>
        val errorPosition = s"$variableCode:${line(expr).getOrElse("")}:$relativeFileName"
        logger.warn(s"ArrayDimFetchExpr without dimensions should be handled in assignment: $errorPosition")
        Ast()
    }
  }

  private def astForNewExpr(expr: PhpNewExpr): Ast = {
    expr.className match {
      case classLikeStmt: PhpClassLikeStmt =>
        astForAnonymousClassInstantiation(expr, classLikeStmt)

      case classNameExpr: PhpExpr =>
        astForSimpleNewExpr(expr, classNameExpr)

      case other =>
        throw new NotImplementedError(s"unexpected expression '$other' of type ${other.getClass}")
    }
  }

  private def astForMatchExpr(expr: PhpMatchExpr): Ast = {
    val conditionAst = astForExpr(expr.condition)

    val matchNode = controlStructureNode(expr, ControlStructureTypes.MATCH, s"match (${conditionAst.rootCodeOrEmpty})")

    val matchBodyBlock = blockNode(expr)
    val armsAsts       = expr.matchArms.flatMap(astsForMatchArm)
    val matchBody      = Ast(matchBodyBlock).withChildren(armsAsts)

    controlStructureAst(matchNode, Option(conditionAst), matchBody :: Nil)
  }

  private def astsForMatchArm(matchArm: PhpMatchArm): List[Ast] = {
    val targetAsts = matchArm.conditions.flatMap { condition =>
      val conditionAst = astForExpr(condition)
      // In PHP cases aren't labeled with `case`, but this is used by the CFG creator to differentiate between
      // case/default labels and other labels.
      val code          = s"case ${conditionAst.rootCode.getOrElse(NameConstants.Unknown)}"
      val jumpTargetAst = Ast(NewJumpTarget().name(code).code(code).lineNumber(line(condition)))
      jumpTargetAst :: conditionAst :: Nil
    }
    val defaultLabel = Option.when(matchArm.isDefault)(
      Ast(NewJumpTarget().name(NameConstants.Default).code(NameConstants.Default).lineNumber(line(matchArm)))
    )

    val bodyAst = astForExpr(matchArm.body)

    targetAsts ++ defaultLabel :+ bodyAst
  }

  private def astForYieldExpr(expr: PhpYieldExpr): Ast = {
    val maybeKey = expr.key.map(astForExpr)
    val maybeVal = expr.value.map(astForExpr)

    val code = (maybeKey, maybeVal) match {
      case (Some(key), Some(value)) =>
        s"yield ${key.rootCodeOrEmpty} => ${value.rootCodeOrEmpty}"

      case _ =>
        s"yield ${maybeKey.map(_.rootCodeOrEmpty).getOrElse("")}${maybeVal.map(_.rootCodeOrEmpty).getOrElse("")}".trim
    }

    val yieldNode = controlStructureNode(expr, ControlStructureTypes.YIELD, code)

    Ast(yieldNode)
      .withChildren(maybeKey.toList)
      .withChildren(maybeVal.toList)
  }

  private def astForClassConstFetchExpr(expr: PhpClassConstFetchExpr): Ast = {
    expr.constantName match {
      // Foo::class should be a TypeRef and not a field access
      case Some(constNameExpr) if constNameExpr.name == NameConstants.Class =>
        astForMagicClassConstant(expr)

      case _ =>
        val targetAst           = astForExpr(expr.className)
        val fieldIdentifierName = expr.constantName.map(_.name).getOrElse(NameConstants.Unknown)
        val fieldIdentifier     = fieldIdentifierNode(expr, fieldIdentifierName, fieldIdentifierName)
        val fieldAccessCode     = s"${targetAst.rootCodeOrEmpty}$StaticMethodDelimiter${fieldIdentifier.code}"
        val fieldAccessCall     = operatorCallNode(expr, fieldAccessCode, Operators.fieldAccess, None)
        callAst(fieldAccessCall, List(targetAst, Ast(fieldIdentifier)))
    }
  }

  private def astForMagicClassConstant(expr: PhpClassConstFetchExpr): Ast = {
    val typeFullName = expr.className match {
      case nameExpr: PhpNameExpr =>
        scope
          .lookupVariable(nameExpr.name)
          .flatMap(_.properties.get(PropertyNames.TypeFullName).map(_.toString))
          .getOrElse(nameExpr.name)

      case expr =>
        logger.warn(s"Unexpected expression as class name in <class>::class expression: $relativeFileName")
        NameConstants.Unknown
    }

    Ast(typeRefNode(expr, s"$typeFullName${MethodDelimiter}class", typeFullName))
  }

  private def astForConstFetchExpr(expr: PhpConstFetchExpr): Ast = {
    val constName = expr.name.name

    if (NameConstants.isBoolean(constName)) {
      Ast(literalNode(expr, constName, TypeConstants.Bool))
    } else if (NameConstants.isNull(constName)) {
      Ast(literalNode(expr, constName, TypeConstants.NullType))
    } else {
      val namespaceName   = NamespaceTraversal.globalNamespaceName
      val identifier      = identifierNode(expr, namespaceName, namespaceName, Defines.Any)
      val fieldIdentifier = fieldIdentifierNode(expr, constName, constName)

      val fieldAccessNode = operatorCallNode(expr, code = constName, Operators.fieldAccess, None)
      val args            = List(identifier, fieldIdentifier).map(Ast(_))

      callAst(fieldAccessNode, args)
    }
  }

  private def astForAnonymousClassInstantiation(expr: PhpNewExpr, classLikeStmt: PhpClassLikeStmt): Ast = {
    val tmpClassNameExpr = PhpNameExpr(this.scope.getNewClassTmp, expr.attributes)

    astForClassLikeStmt(classLikeStmt.copy(name = Some(tmpClassNameExpr)))
    astForSimpleNewExpr(expr, tmpClassNameExpr)
  }

  private def astForSimpleNewExpr(expr: PhpNewExpr, classNameExpr: PhpExpr): Ast = {
    val (maybeNameAst, className) = classNameExpr match {
      case nameExpr: PhpNameExpr =>
        (None, nameExpr.name)

      case expr: PhpExpr =>
        val ast = astForExpr(expr)
        // The name doesn't make sense in this case, but the AST will be more useful
        val name = ast.rootCode.getOrElse(NameConstants.Unknown)
        (Option(ast), name)
    }

    val tmpIdentifier = getTmpIdentifier(expr, Option(className))
    val local         = handleVariableOccurrence(expr, tmpIdentifier.name)

    // Alloc assign
    val allocCode       = s"$className.<alloc>()"
    val allocNode       = operatorCallNode(expr, allocCode, Operators.alloc, Option(className))
    val allocAst        = callAst(allocNode, base = maybeNameAst)
    val allocAssignCode = s"${tmpIdentifier.code} = ${allocAst.rootCodeOrEmpty}"
    val allocAssignNode = operatorCallNode(expr, allocAssignCode, Operators.assignment, Option(className))
    val allocAssignAst =
      callAst(allocAssignNode, astForIdentifierWithLocalRef(tmpIdentifier, local) :: allocAst :: Nil)

    // Init node
    val initArgs      = expr.args.map(astForCallArg)
    val initFullName  = s"$className$MethodDelimiter$ConstructorMethodName"
    val initCode      = s"new $className(${initArgs.map(_.rootCodeOrEmpty).mkString(",")})"
    val maybeTypeHint = scope.resolveIdentifier(className).map(_.name) // consider imported or defined types
    val initCallNode = callNode(
      expr,
      initCode,
      ConstructorMethodName,
      initFullName,
      DispatchTypes.STATIC_DISPATCH,
      None,
      maybeTypeHint.orElse(Some(Defines.Any)) // TODO Review Note: Should the hint be under dynamicTypeHintFullName?
    )
    val initReceiver = astForIdentifierWithLocalRef(tmpIdentifier.copy, local)
    val initCallAst  = callAst(initCallNode, initArgs, base = Option(initReceiver))

    // Return identifier
    val returnIdentifierAst = astForIdentifierWithLocalRef(tmpIdentifier.copy, local)
    Ast(blockNode(expr, "", Defines.Any))
      .withChild(allocAssignAst)
      .withChild(initCallAst)
      .withChild(returnIdentifierAst)
  }

}
