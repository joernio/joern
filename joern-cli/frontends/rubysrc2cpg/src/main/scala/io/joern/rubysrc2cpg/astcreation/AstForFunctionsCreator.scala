package io.joern.rubysrc2cpg.astcreation

import io.joern.rubysrc2cpg.parser.RubyParser.*
import io.joern.rubysrc2cpg.passes.Defines
import io.joern.rubysrc2cpg.utils.PackageContext
import io.joern.x2cpg.{Ast, ValidationMode, Defines as XDefines}
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.codepropertygraph.generated.{EdgeTypes, ModifierTypes}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

trait AstForFunctionsCreator(packageContext: PackageContext)(implicit withSchemaValidation: ValidationMode) {
  this: AstCreator =>

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
        astForMethodIdentifierContext(ctx.methodIdentifier(), text(ctx))
    val callNode = astMethodName.head.nodes.filter(node => node.isInstanceOf[NewCall]).head.asInstanceOf[NewCall]

    // Create thisParameter if this is an instance method
    // TODO may need to revisit to make this more robust

    val (methodName, methodFullName) = if (callNode.name == Defines.Initialize) {
      (XDefines.ConstructorMethodName, classStack.reverse :+ XDefines.ConstructorMethodName mkString pathSep)
    } else {
      (callNode.name, classStack.reverse :+ callNode.name mkString pathSep)
    }
    val newMethodNode = methodNode(ctx, methodName, text(ctx), methodFullName, None, relativeFilename)
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

}
