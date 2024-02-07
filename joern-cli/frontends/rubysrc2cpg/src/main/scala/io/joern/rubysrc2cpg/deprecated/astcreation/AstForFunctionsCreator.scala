package io.joern.rubysrc2cpg.deprecated.astcreation

import io.joern.rubysrc2cpg.deprecated.parser.DeprecatedRubyParser.*
import io.joern.rubysrc2cpg.deprecated.passes.Defines
import io.joern.rubysrc2cpg.deprecated.utils.PackageContext
import io.joern.x2cpg.utils.NodeBuilders.newModifierNode
import io.joern.x2cpg.{Ast, ValidationMode, Defines as XDefines}
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, EdgeTypes, ModifierTypes}
import org.antlr.v4.runtime.ParserRuleContext
import org.antlr.v4.runtime.tree.TerminalNode
import org.slf4j.LoggerFactory

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.jdk.CollectionConverters.*

trait AstForFunctionsCreator(implicit withSchemaValidation: ValidationMode) {
  this: AstCreator =>

  private val logger = LoggerFactory.getLogger(getClass)

  /*
   *Fake methods created from yield blocks and their yield calls will have this suffix in their names
   */
  protected val YIELD_SUFFIX = "_yield"

  /*
   * This is used to mark call nodes created due to yield calls. This is set in their names at creation.
   * The appropriate name wrt the names of their actual methods is set later in them.
   */
  protected val UNRESOLVED_YIELD = "unresolved_yield"

  /*
   * Stack of variable identifiers incorrectly identified as method identifiers
   * Each AST contains exactly one call or identifier node
   */
  protected val methodNameAsIdentifierStack: mutable.Stack[Ast]        = mutable.Stack.empty
  protected val methodAliases: mutable.HashMap[String, String]         = mutable.HashMap.empty
  protected val methodNameToMethod: mutable.HashMap[String, NewMethod] = mutable.HashMap.empty
  protected val methodDefInArgument: ListBuffer[Ast]                   = ListBuffer.empty
  protected val methodNamesWithYield: mutable.HashSet[String]          = mutable.HashSet.empty
  protected val blockMethods: ListBuffer[Ast]                          = ListBuffer.empty

  /** @return
    *   the method name if found as an alias, or the given name if not found.
    */
  protected def resolveAlias(name: String): String = {
    methodAliases.getOrElse(name, name)
  }

  protected def astForMethodDefinitionContext(ctx: MethodDefinitionContext): Seq[Ast] = {
    val astMethodName = Option(ctx.methodNamePart()) match
      case Some(ctxMethodNamePart) =>
        astForMethodNamePartContext(ctxMethodNamePart)
      case None =>
        astForMethodIdentifierContext(ctx.methodIdentifier(), code(ctx))
    val callNode = astMethodName.head.nodes.filter(node => node.isInstanceOf[NewCall]).head.asInstanceOf[NewCall]

    // Create thisParameter if this is an instance method
    // TODO may need to revisit to make this more robust

    val (methodName, methodFullName) = if (callNode.name == Defines.Initialize) {
      (XDefines.ConstructorMethodName, classStack.reverse :+ XDefines.ConstructorMethodName mkString pathSep)
    } else {
      (callNode.name, classStack.reverse :+ callNode.name mkString pathSep)
    }
    val newMethodNode = methodNode(ctx, methodName, code(ctx), methodFullName, None, relativeFilename)
      .columnNumber(callNode.columnNumber)
      .lineNumber(callNode.lineNumber)

    scope.pushNewScope(newMethodNode)

    val astMethodParamSeq = ctx.methodNamePart() match {
      case _: SimpleMethodNamePartContext if !classStack.top.endsWith(":program") =>
        val thisParameterNode = createMethodParameterIn(
          "this",
          typeFullName = callNode.methodFullName,
          lineNumber = callNode.lineNumber,
          colNumber = callNode.columnNumber,
          index = 0,
          order = 0
        )
        Seq(Ast(thisParameterNode)) ++ astForMethodParameterPartContext(ctx.methodParameterPart())
      case _ => astForMethodParameterPartContext(ctx.methodParameterPart())
    }

    Option(ctx.END()).foreach(endNode => newMethodNode.lineNumberEnd(endNode.getSymbol.getLine))

    callNode.methodFullName(methodFullName)

    val classType = if (classStack.isEmpty) "Standalone" else classStack.top
    val classPath = classStack.reverse.toList.mkString(pathSep)
    packageContext.packageTable.addPackageMethod(packageContext.moduleName, callNode.name, classPath, classType)

    val astBody = Option(ctx.bodyStatement()) match {
      case Some(ctxBodyStmt) => astForBodyStatementContext(ctxBodyStmt, true)
      case None =>
        val expAst = astForExpressionContext(ctx.expression())
        Seq(lastStmtAsReturnAst(ctx, expAst.head, Option(text(ctx.expression()))))
    }

    // process yield calls.
    astBody
      .flatMap(_.nodes.collect { case x: NewCall => x }.filter(_.name == UNRESOLVED_YIELD))
      .foreach { yieldCallNode =>
        val name           = newMethodNode.name
        val methodFullName = classStack.reverse :+ callNode.name mkString pathSep
        yieldCallNode.name(name + YIELD_SUFFIX)
        yieldCallNode.methodFullName(methodFullName + YIELD_SUFFIX)
        methodNamesWithYield.add(newMethodNode.name)
        /*
         * These are calls to the yield block of this method.
         * Add this method to the list of yield blocks.
         * The add() is idempotent and so adding the same method multiple times makes no difference.
         * It just needs to be added at this place so that it gets added iff it has a yield block
         */
      }

    val methodRetNode = NewMethodReturn().typeFullName(Defines.Any)

    val modifierNode = lastModifier match {
      case Some(modifier) => NewModifier().modifierType(modifier).code(modifier)
      case None           => NewModifier().modifierType(ModifierTypes.PUBLIC).code(ModifierTypes.PUBLIC)
    }
    /*
     * public/private/protected modifiers are in a separate statement
     * TODO find out how they should be used. Need to do this iff it adds any value
     */
    if (methodName != XDefines.ConstructorMethodName) {
      methodNameToMethod.put(newMethodNode.name, newMethodNode)
    }

    /* Before creating ast, we traverse the method params and identifiers and link them*/
    val identifiers =
      astBody.flatMap(ast => ast.nodes.filter(_.isInstanceOf[NewIdentifier])).asInstanceOf[Seq[NewIdentifier]]

    val params = astMethodParamSeq
      .flatMap(_.nodes.collect { case x: NewMethodParameterIn => x })
      .toList
    val locals = scope.createAndLinkLocalNodes(diffGraph, params.map(_.name).toSet)

    params.foreach { param =>
      identifiers.filter(_.name == param.name).foreach { identifier =>
        diffGraph.addEdge(identifier, param, EdgeTypes.REF)
      }
    }
    scope.popScope()

    Seq(
      methodAst(
        newMethodNode,
        astMethodParamSeq,
        blockAst(blockNode(ctx), locals.map(Ast.apply) ++ astBody.toList),
        methodRetNode,
        Seq[NewModifier](modifierNode)
      )
    )
  }

  private def astForOperatorMethodNameContext(ctx: OperatorMethodNameContext): Seq[Ast] = {
    /*
     * This is for operator overloading for the class
     */
    val name           = code(ctx)
    val methodFullName = classStack.reverse :+ name mkString pathSep

    val node = callNode(ctx, code(ctx), name, methodFullName, DispatchTypes.STATIC_DISPATCH, None, Option(Defines.Any))
    ctx.children.asScala
      .collectFirst { case x: TerminalNode => x }
      .foreach(x => node.lineNumber(x.lineNumber).columnNumber(x.columnNumber))
    Seq(callAst(node))
  }

  protected def astForMethodNameContext(ctx: MethodNameContext): Seq[Ast] = {
    if (ctx.methodIdentifier() != null) {
      astForMethodIdentifierContext(ctx.methodIdentifier(), code(ctx))
    } else if (ctx.operatorMethodName() != null) {
      astForOperatorMethodNameContext(ctx.operatorMethodName)
    } else if (ctx.keyword() != null) {
      val node =
        callNode(ctx, code(ctx), code(ctx), code(ctx), DispatchTypes.STATIC_DISPATCH, None, Option(Defines.Any))
      ctx.children.asScala
        .collectFirst { case x: TerminalNode => x }
        .foreach(x =>
          node.lineNumber(x.lineNumber).columnNumber(x.columnNumber).name(x.getText).methodFullName(x.getText)
        )
      Seq(callAst(node))
    } else {
      Seq.empty
    }
  }

  private def astForSingletonMethodNamePartContext(ctx: SingletonMethodNamePartContext): Seq[Ast] = {
    val definedMethodNameAst = astForDefinedMethodNameContext(ctx.definedMethodName())
    val singletonObjAst      = astForSingletonObjectContext(ctx.singletonObject())
    definedMethodNameAst ++ singletonObjAst
  }

  private def astForSingletonObjectContext(ctx: SingletonObjectContext): Seq[Ast] = {
    if (ctx.variableIdentifier() != null) {
      Seq(astForVariableIdentifierHelper(ctx.variableIdentifier(), true))
    } else if (ctx.pseudoVariableIdentifier() != null) {
      Seq(Ast())
    } else if (ctx.expressionOrCommand() != null) {
      astForExpressionOrCommand(ctx.expressionOrCommand())
    } else {
      Seq.empty
    }
  }

  private def astForParametersContext(ctx: ParametersContext): Seq[Ast] = {
    if (ctx == null) return Seq()

    // the parameterTupleList holds the parameter terminal node and is the parameter a variadic parameter
    val parameterTupleList = ctx.parameter().asScala.map {
      case procCtx if procCtx.procParameter() != null =>
        (Option(procCtx.procParameter().LOCAL_VARIABLE_IDENTIFIER()), false)
      case optCtx if optCtx.optionalParameter() != null =>
        (Option(optCtx.optionalParameter().LOCAL_VARIABLE_IDENTIFIER()), false)
      case manCtx if manCtx.mandatoryParameter() != null =>
        (Option(manCtx.mandatoryParameter().LOCAL_VARIABLE_IDENTIFIER()), false)
      case arrCtx if arrCtx.arrayParameter() != null =>
        (Option(arrCtx.arrayParameter().LOCAL_VARIABLE_IDENTIFIER()), arrCtx.arrayParameter().STAR() != null)
      case keywordCtx if keywordCtx.keywordParameter() != null =>
        (Option(keywordCtx.keywordParameter().LOCAL_VARIABLE_IDENTIFIER()), false)
      case _ => (None, false)
    }

    parameterTupleList.zipWithIndex.map { case (paraTuple, paraIndex) =>
      paraTuple match
        case (Some(paraValue), isVariadic) =>
          val varSymbol = paraValue.getSymbol
          createIdentifierWithScope(ctx, varSymbol.getText, varSymbol.getText, Defines.Any, Seq[String](Defines.Any))
          Ast(
            createMethodParameterIn(
              varSymbol.getText,
              lineNumber = Some(varSymbol.getLine),
              colNumber = Some(varSymbol.getCharPositionInLine),
              order = paraIndex + 1,
              index = paraIndex + 1
            ).isVariadic(isVariadic)
          )
        case _ =>
          Ast(
            createMethodParameterIn(
              getUnusedVariableNames(usedVariableNames, Defines.TempParameter),
              order = paraIndex + 1,
              index = paraIndex + 1
            )
          )
    }.toList
  }

  // TODO: Rewrite for simplicity and take into account more than parameter names.
  private def astForMethodParameterPartContext(ctx: MethodParameterPartContext): Seq[Ast] = {
    if (ctx == null || ctx.parameters == null) Seq.empty
    else astForParametersContext(ctx.parameters)
  }

  private def astForDefinedMethodNameContext(ctx: DefinedMethodNameContext): Seq[Ast] = {
    Option(ctx.methodName()) match
      case Some(methodNameCtx) => astForMethodNameContext(methodNameCtx)
      case None                => astForAssignmentLikeMethodIdentifierContext(ctx.assignmentLikeMethodIdentifier())
  }

  private def astForAssignmentLikeMethodIdentifierContext(ctx: AssignmentLikeMethodIdentifierContext): Seq[Ast] = {
    Seq(
      callAst(
        callNode(ctx, code(ctx), code(ctx), code(ctx), DispatchTypes.STATIC_DISPATCH, Some(""), Some(Defines.Any))
      )
    )
  }

  private def astForMethodNamePartContext(ctx: MethodNamePartContext): Seq[Ast] = ctx match {
    case ctx: SimpleMethodNamePartContext    => astForSimpleMethodNamePartContext(ctx)
    case ctx: SingletonMethodNamePartContext => astForSingletonMethodNamePartContext(ctx)
    case _ =>
      logger.error(s"astForMethodNamePartContext() $relativeFilename, ${text(ctx)} All contexts mismatched.")
      Seq(Ast())
  }

  private def astForSimpleMethodNamePartContext(ctx: SimpleMethodNamePartContext): Seq[Ast] =
    astForDefinedMethodNameContext(ctx.definedMethodName)

  protected def methodForClosureStyleFn(ctx: ParserRuleContext): NewMethod = {
    val procMethodName = s"proc_${blockIdCounter.getAndAdd(1)}"
    val methodFullName = classStack.reverse :+ procMethodName mkString pathSep
    methodNode(ctx, procMethodName, code(ctx), methodFullName, None, relativeFilename)
  }

  protected def astForProcDefinitionContext(ctx: ProcDefinitionContext): Seq[Ast] = {
    /*
     * Model a proc as a method
     */
    // Note: For parameters in the Proc definition, an implicit parameter which goes by the name of `this` is added to the cpg
    val newMethodNode = methodForClosureStyleFn(ctx)

    scope.pushNewScope(newMethodNode)

    val astMethodParam = astForParametersContext(ctx.parameters())
    val paramNames     = astMethodParam.flatMap(_.nodes).collect { case x: NewMethodParameterIn => x.name }.toSet
    val astBody        = astForCompoundStatement(ctx.block.compoundStatement, true)
    val locals         = scope.createAndLinkLocalNodes(diffGraph, paramNames).map(Ast.apply)

    val methodRetNode = NewMethodReturn()
      .typeFullName(Defines.Any)

    val modifiers = newModifierNode(ModifierTypes.PUBLIC) :: newModifierNode(ModifierTypes.LAMBDA) :: Nil

    val methAst = methodAst(
      newMethodNode,
      astMethodParam,
      blockAst(blockNode(ctx), locals ++ astBody.toList),
      methodRetNode,
      modifiers
    )
    blockMethods.addOne(methAst)

    val callArgs = astMethodParam
      .flatMap(_.root)
      .collect { case x: NewMethodParameterIn => x }
      .map(param => Ast(createIdentifierWithScope(ctx, param.name, param.code, Defines.Any, Seq(), true)))

    val procCallNode =
      callNode(
        ctx,
        code(ctx),
        newMethodNode.name,
        newMethodNode.fullName,
        DispatchTypes.STATIC_DISPATCH,
        None,
        Option(Defines.Any)
      )

    scope.popScope()

    Seq(callAst(procCallNode, callArgs))
  }

  def astForDefinedMethodNameOrSymbolContext(ctx: DefinedMethodNameOrSymbolContext): Seq[Ast] =
    if (ctx == null) {
      Seq.empty
    } else {
      if (ctx.definedMethodName() != null) {
        astForDefinedMethodNameContext(ctx.definedMethodName())
      } else {
        Seq(astForSymbolLiteral(ctx.symbol()))
      }
    }

  protected def astForBlockFunction(
    ctxStmt: StatementsContext,
    ctxParam: Option[BlockParameterContext],
    blockMethodName: String,
    lineStart: Int,
    lineEnd: Int,
    colStart: Int,
    colEnd: Int
  ): Seq[Ast] = {
    /*
     * Model a block as a method
     */
    val methodFullName = classStack.reverse :+ blockMethodName mkString pathSep
    val newMethodNode = methodNode(ctxStmt, blockMethodName, code(ctxStmt), methodFullName, None, relativeFilename)
      .lineNumber(lineStart)
      .lineNumberEnd(lineEnd)
      .columnNumber(colStart)
      .columnNumberEnd(colEnd)

    scope.pushNewScope(newMethodNode)
    val astMethodParam = ctxParam.map(astForBlockParameterContext).getOrElse(Seq())

    val publicModifier = NewModifier().modifierType(ModifierTypes.PUBLIC)
    val paramSeq = astMethodParam.flatMap(_.root).map {
      /* In majority of cases, node will be an identifier */
      case identifierNode: NewIdentifier =>
        val param = NewMethodParameterIn()
          .name(identifierNode.name)
          .code(identifierNode.code)
          .typeFullName(identifierNode.typeFullName)
          .lineNumber(identifierNode.lineNumber)
          .columnNumber(identifierNode.columnNumber)
          .dynamicTypeHintFullName(identifierNode.dynamicTypeHintFullName)
        Ast(param)
      case _: NewCall =>
        /* TODO: Occasionally, we might encounter a _ call in cases like "do |_, x|" where we should handle this?
         * But for now, we just return an empty AST. Keeping this match explicitly here so we come back */
        Ast()
      case _ =>
        Ast()
    }
    val paramNames = (astMethodParam ++ paramSeq)
      .flatMap(_.root)
      .collect {
        case x: NewMethodParameterIn => x.name
        case x: NewIdentifier        => x.name
      }
      .toSet
    val astBody       = astForStatements(ctxStmt, true)
    val locals        = scope.createAndLinkLocalNodes(diffGraph, paramNames).map(Ast.apply)
    val methodRetNode = NewMethodReturn().typeFullName(Defines.Any)

    scope.popScope()

    Seq(
      methodAst(
        newMethodNode,
        paramSeq,
        blockAst(blockNode(ctxStmt), locals ++ astBody.toList),
        methodRetNode,
        Seq(publicModifier)
      )
    )
  }

}
