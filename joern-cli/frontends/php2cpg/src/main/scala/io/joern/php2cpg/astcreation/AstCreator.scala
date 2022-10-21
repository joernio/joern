package io.joern.php2cpg.astcreation

import io.joern.php2cpg.astcreation.AstCreator.{NameConstants, TypeConstants, operatorSymbols}
import io.joern.php2cpg.datastructures.{ArrayIndexTracker, Scope}
import io.joern.php2cpg.parser.Domain._
import io.joern.x2cpg.Ast.storeInDiffGraph
import io.joern.x2cpg.datastructures.Global
import io.joern.x2cpg.{Ast, AstCreatorBase, Defines}
import io.joern.x2cpg.utils.NodeBuilders.{fieldIdentifierNode, identifierNode, modifierNode, operatorCallNode}
import io.shiftleft.codepropertygraph.generated._
import io.shiftleft.codepropertygraph.generated.nodes.Call.PropertyDefaults
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.passes.IntervalKeyPool
import org.slf4j.LoggerFactory
import overflowdb.BatchedUpdate

class AstCreator(filename: String, phpAst: PhpFile, global: Global) extends AstCreatorBase(filename) {

  private val logger = LoggerFactory.getLogger(AstCreator.getClass)
  private val scope  = new Scope()

  private val tmpKeyPool            = new IntervalKeyPool(first = 0, last = Long.MaxValue)
  private def getNewTmpName: String = s"tmp${tmpKeyPool.next.toString}"

  override def createAst(): BatchedUpdate.DiffGraphBuilder = {
    val ast = astForPhpFile(phpAst)
    storeInDiffGraph(ast, diffGraph)
    diffGraph
  }

  private def expectSingle(asts: List[Ast]): Ast = {
    asts match {
      case Nil =>
        logger.warn(s"expectSingle found no asts. Returning emtpy AST in $filename")
        Ast()

      case single :: Nil => single

      case head :: _ =>
        logger.warn(s"expectSingle found multiple astas. Returning first in $filename")
        head
    }
  }

  private def registerType(typ: String): String = {
    global.usedTypes.putIfAbsent(typ, true)
    typ
  }

  private def astForPhpFile(file: PhpFile): Ast = {
    val namespaceBlock = globalNamespaceBlock().filename(absolutePath(filename))

    scope.pushNewScope(namespaceBlock)

    val globalTypeDecl = NewTypeDecl()
      .name(namespaceBlock.name)
      .fullName(namespaceBlock.fullName)
      .astParentFullName(namespaceBlock.fullName)
      .code(namespaceBlock.code)
      .filename(filename)

    scope.pushNewScope(globalTypeDecl)

    val globalMethod = NewMethod()
      .name(globalTypeDecl.name)
      .fullName(globalTypeDecl.fullName)
      .astParentFullName(globalTypeDecl.fullName)
      .code(globalTypeDecl.code)

    scope.pushNewScope(globalMethod)

    val (fileConsts, otherStmts) = file.children.partition(_.isInstanceOf[PhpConstStmt])
    val constAsts                = fileConsts.flatMap(fc => astsForConstStmt(fc.asInstanceOf[PhpConstStmt]))
    val methodChildren           = otherStmts.map(astForStmt)
    val clinitAst                = astForStaticAndConstInits

    scope.popScope() // Global method
    scope.popScope() // Global typeDecl
    scope.popScope() // Global namespace

    val globalMethodAst =
      Ast(globalMethod)
        .withChild(Ast(NewBlock()).withChildren(methodChildren))
        .withChild(Ast(NewMethodReturn()))

    val globalTypeDeclAst =
      Ast(globalTypeDecl)
        .withChildren(constAsts)
        .withChildren(clinitAst.toList)
        .withChild(globalMethodAst)

    Ast(namespaceBlock).withChild(globalTypeDeclAst)
  }

  private def astForStmt(stmt: PhpStmt): Ast = {
    stmt match {
      case echoStmt: PhpEchoStmt       => astForEchoStmt(echoStmt)
      case methodDecl: PhpMethodDecl   => astForMethodDecl(methodDecl)
      case expr: PhpExpr               => astForExpr(expr)
      case breakStmt: PhpBreakStmt     => astForBreakStmt(breakStmt)
      case contStmt: PhpContinueStmt   => astForContinueStmt(contStmt)
      case whileStmt: PhpWhileStmt     => astForWhileStmt(whileStmt)
      case doStmt: PhpDoStmt           => astForDoStmt(doStmt)
      case forStmt: PhpForStmt         => astForForStmt(forStmt)
      case ifStmt: PhpIfStmt           => astForIfStmt(ifStmt)
      case switchStmt: PhpSwitchStmt   => astForSwitchStmt(switchStmt)
      case tryStmt: PhpTryStmt         => astForTryStmt(tryStmt)
      case returnStmt: PhpReturnStmt   => astForReturnStmt(returnStmt)
      case classStmt: PhpClassStmt     => astForClassStmt(classStmt)
      case gotoStmt: PhpGotoStmt       => astForGotoStmt(gotoStmt)
      case labelStmt: PhpLabelStmt     => astForLabelStmt(labelStmt)
      case namespace: PhpNamespaceStmt => astForNamespaceStmt(namespace)
      case _: NopStmt                  => Ast() // TODO This'll need to be updated when comments are added.

      case haltStmt: PhpHaltCompilerStmt => astForHaltCompilerStmt(haltStmt)
      case null =>
        logger.warn("stmt was null")
        ???
    }
  }

  private def astForEchoStmt(echoStmt: PhpEchoStmt): Ast = {
    val args     = echoStmt.exprs.map(astForExpr)
    val code     = s"echo ${args.map(rootCode(_)).mkString(",")}"
    val callNode = operatorCallNode("echo", code, line = line(echoStmt))
    callAst(callNode, args)
  }

  private def thisParamAstForMethod(lineNumber: Option[Integer]): Ast = {
    val typeFullName = scope.getEnclosingTypeDeclType.getOrElse(TypeConstants.Any)
    val thisNode = NewMethodParameterIn()
      .name(NameConstants.This)
      .code(NameConstants.This)
      .evaluationStrategy(EvaluationStrategies.BY_SHARING)
      .typeFullName(typeFullName)
      .dynamicTypeHintFullName(typeFullName :: Nil)
      .lineNumber(lineNumber)
      .index(0)

    scope.addToScope(NameConstants.This, thisNode)

    Ast(thisNode)
  }

  private def thisIdentifier(lineNumber: Option[Integer]): NewIdentifier = {
    val typ = scope.getEnclosingTypeDeclType

    identifierNode(NameConstants.This, typ, dynamicTypeHintFullName = typ.toList, line = lineNumber)
      .code("$" + NameConstants.This)
  }

  private def setParamIndices(asts: Seq[Ast]): Seq[Ast] = {
    asts.map(_.root).zipWithIndex.foreach {
      case (Some(root: NewMethodParameterIn), idx) =>
        root.index(idx + 1)

      case (root, _) =>
        logger.warn(s"Trying to set index for unsupported node $root")
    }

    asts
  }

  private def astForMethodDecl(decl: PhpMethodDecl, bodyPrefixAsts: List[Ast] = Nil): Ast = {
    val namespacePrefix = getNamespacePrefixForName

    val methodNode =
      NewMethod()
        .name(decl.name.name)
        .fullName(s"$namespacePrefix${decl.name.name}")
        .code(decl.name.name)
        .lineNumber(line(decl))
        .isExternal(false)

    scope.pushNewScope(methodNode)

    val returnType = decl.returnType.map(_.name).getOrElse(TypeConstants.Any)

    val modifiers = decl.modifiers.map(modifierNode)
    val thisParam = if (decl.isClassMethod && !modifiers.exists(_.modifierType == ModifierTypes.STATIC)) {
      Some(thisParamAstForMethod(line(decl)))
    } else {
      None
    }
    val parameters      = thisParam.toList ++ setParamIndices(decl.params.map(astForParam))
    val methodBodyStmts = bodyPrefixAsts ++ decl.stmts.map(astForStmt)
    val methodReturn    = methodReturnNode(returnType, line = line(decl), column = None)

    val declLocals = scope.getLocalsInScope.map(Ast(_))
    val methodBody = blockAst(NewBlock(), declLocals ++ methodBodyStmts)

    scope.popScope()
    methodAstWithAnnotations(methodNode, parameters, methodBody, methodReturn, modifiers)
  }

  private def stmtBlockAst(stmts: Seq[PhpStmt], lineNumber: Option[Integer]): Ast = {
    val bodyBlock    = NewBlock().lineNumber(lineNumber)
    val bodyStmtAsts = stmts.map(astForStmt)
    Ast(bodyBlock).withChildren(bodyStmtAsts)
  }

  private def astForParam(param: PhpParam): Ast = {
    val evaluationStrategy =
      if (param.byRef)
        EvaluationStrategies.BY_REFERENCE
      else
        EvaluationStrategies.BY_VALUE

    val typeFullName = param.paramType.map(_.name).getOrElse(TypeConstants.Any)

    val byRefCodePrefix = if (param.byRef) "&" else ""
    val code            = byRefCodePrefix + "$" + param.name
    val paramNode = NewMethodParameterIn()
      .name(param.name)
      .code(code)
      .lineNumber(line(param))
      .isVariadic(param.isVariadic)
      .evaluationStrategy(evaluationStrategy)
      .typeFullName(typeFullName)

    scope.addToScope(param.name, paramNode)

    Ast(paramNode)
  }

  private def astForExpr(expr: PhpExpr): Ast = {
    expr match {
      case funcCallExpr: PhpCallExpr => astForCall(funcCallExpr)
      case variableExpr: PhpVariable => astForVariableExpr(variableExpr)
      case nameExpr: PhpNameExpr     => astForNameExpr(nameExpr)
      case assignExpr: PhpAssignment => astForAssignment(assignExpr)
      case scalarExpr: PhpScalar     => astForScalar(scalarExpr)
      case binaryOp: PhpBinaryOp     => astForBinOp(binaryOp)
      case unaryOp: PhpUnaryOp       => astForUnaryOp(unaryOp)
      case castExpr: PhpCast         => astForCastExpr(castExpr)
      case issetExpr: PhpIsset       => astForIssetExpr(issetExpr)
      case printExpr: PhpPrint       => astForPrintExpr(printExpr)
      case ternaryOp: PhpTernaryOp   => astForTernaryOp(ternaryOp)
      case throwExpr: PhpThrowExpr   => astForThrow(throwExpr)
      case cloneExpr: PhpCloneExpr   => astForClone(cloneExpr)
      case emptyExpr: PhpEmptyExpr   => astForEmpty(emptyExpr)
      case evalExpr: PhpEvalExpr     => astForEval(evalExpr)
      case exitExpr: PhpExitExpr     => astForExit(exitExpr)
      case arrayExpr: PhpArrayExpr   => astForArrayExpr(arrayExpr)

      case classConstFetchExpr: PhpClassConstFetchExpr => astForClassConstFetchExpr(classConstFetchExpr)
      case arrayDimFetchExpr: PhpArrayDimFetchExpr     => astForArrayDimFetchExpr(arrayDimFetchExpr)
      case errorSuppressExpr: PhpErrorSuppressExpr     => astForErrorSuppressExpr(errorSuppressExpr)
      case instanceOfExpr: PhpInstanceOfExpr           => astForInstanceOfExpr(instanceOfExpr)
      case propertyFetchExpr: PhpPropertyFetchExpr     => astForPropertyFetchExpr(propertyFetchExpr)

      case null =>
        logger.warn("expr was null")
        ???
    }
  }

  private def intToLiteralAst(num: Int): Ast = {
    Ast(NewLiteral().code(num.toString).typeFullName(TypeConstants.Int))
  }

  private def astForBreakStmt(breakStmt: PhpBreakStmt): Ast = {
    val code = breakStmt.num.map(num => s"break($num)").getOrElse("break")
    val breakNode = NewControlStructure()
      .controlStructureType(ControlStructureTypes.BREAK)
      .code(code)
      .lineNumber(line(breakStmt))

    val argument = breakStmt.num.map(intToLiteralAst)

    controlStructureAst(breakNode, None, argument.toList)
  }

  private def astForContinueStmt(continueStmt: PhpContinueStmt): Ast = {
    val code = continueStmt.num.map(num => s"continue($num)").getOrElse("continue")
    val continueNode = NewControlStructure()
      .controlStructureType(ControlStructureTypes.CONTINUE)
      .code(code)
      .lineNumber(line(continueStmt))

    val argument = continueStmt.num.map(intToLiteralAst)

    controlStructureAst(continueNode, None, argument.toList)
  }

  private def astForWhileStmt(whileStmt: PhpWhileStmt): Ast = {
    val condition = astForExpr(whileStmt.cond)

    val whileNode = NewControlStructure()
      .controlStructureType(ControlStructureTypes.WHILE)
      .code(s"while (${rootCode(condition)})")
      .lineNumber(line(whileStmt))

    val body = stmtBlockAst(whileStmt.stmts, line(whileStmt))

    controlStructureAst(whileNode, Some(condition), List(body))
  }

  private def astForDoStmt(doStmt: PhpDoStmt): Ast = {
    val condition = astForExpr(doStmt.cond)

    val whileNode = NewControlStructure()
      .controlStructureType(ControlStructureTypes.DO)
      .code(s"do {...} while (${rootCode(condition)})")
      .lineNumber(line(doStmt))

    val body = stmtBlockAst(doStmt.stmts, line(doStmt))

    controlStructureAst(whileNode, Some(condition), List(body), placeConditionLast = true)
  }

  private def astForForStmt(stmt: PhpForStmt): Ast = {
    val lineNumber = line(stmt)

    val initAsts      = stmt.inits.map(astForExpr)
    val conditionAsts = stmt.conditions.map(astForExpr)
    val loopExprAsts  = stmt.loopExprs.map(astForExpr)

    val bodyAst = stmtBlockAst(stmt.bodyStmts, line(stmt))

    val initCode      = initAsts.map(rootCode(_)).mkString(",")
    val conditionCode = conditionAsts.map(rootCode(_)).mkString(",")
    val loopExprCode  = loopExprAsts.map(rootCode(_)).mkString(",")
    val forCode       = s"for ($initCode;$conditionCode;$loopExprCode)"

    val forNode =
      NewControlStructure().controlStructureType(ControlStructureTypes.FOR).lineNumber(lineNumber).code(forCode)
    forAst(forNode, Nil, initAsts, conditionAsts, loopExprAsts, bodyAst)
  }

  private def astForIfStmt(ifStmt: PhpIfStmt): Ast = {
    val condition = astForExpr(ifStmt.cond)

    val thenAst = stmtBlockAst(ifStmt.stmts, line(ifStmt))

    val elseAst = ifStmt.elseIfs match {
      case Nil => ifStmt.elseStmt.map(els => stmtBlockAst(els.stmts, line(els))).toList

      case elseIf :: rest =>
        val newIfStmt     = PhpIfStmt(elseIf.cond, elseIf.stmts, rest, ifStmt.elseStmt, elseIf.attributes)
        val wrappingBlock = NewBlock().lineNumber(line(elseIf))
        val wrappedAst    = Ast(wrappingBlock).withChild(astForIfStmt(newIfStmt)) :: Nil
        wrappedAst
    }

    val conditionCode = rootCode(condition)
    val ifNode = NewControlStructure()
      .controlStructureType(ControlStructureTypes.IF)
      .code(s"if ($conditionCode)")
      .lineNumber(line(ifStmt))

    controlStructureAst(ifNode, Some(condition), thenAst :: elseAst)
  }

  private def astForSwitchStmt(stmt: PhpSwitchStmt): Ast = {
    val conditionAst = astForExpr(stmt.condition)

    val switchNode = NewControlStructure()
      .controlStructureType(ControlStructureTypes.SWITCH)
      .code(s"switch (${rootCode(conditionAst)})")
      .lineNumber(line(stmt))

    val switchBodyBlock = NewBlock().lineNumber(line(stmt))
    val entryAsts       = stmt.cases.flatMap(astsForSwitchCase)
    val switchBody      = Ast(switchBodyBlock).withChildren(entryAsts)

    controlStructureAst(switchNode, Some(conditionAst), switchBody :: Nil)
  }

  private def astForTryStmt(stmt: PhpTryStmt): Ast = {
    val tryBody     = stmtBlockAst(stmt.stmts, line(stmt))
    val catches     = stmt.catches.map(astForCatchStmt)
    val finallyBody = stmt.finallyStmt.map(fin => stmtBlockAst(fin.stmts, line(fin)))

    val tryNode = NewControlStructure()
      .controlStructureType(ControlStructureTypes.TRY)
      .code("TODO")
      .lineNumber(line(stmt))

    tryCatchAst(tryNode, tryBody, catches, finallyBody)
  }

  private def astForReturnStmt(stmt: PhpReturnStmt): Ast = {
    val maybeExprAst = stmt.expr.map(astForExpr)
    val code         = s"return ${maybeExprAst.map(rootCode(_)).getOrElse("")}"

    val returnNode = NewReturn()
      .code(code)
      .lineNumber(line(stmt))

    returnAst(returnNode, maybeExprAst.toList)
  }

  private def astForClassStmt(stmt: PhpClassStmt): Ast = {
    stmt.name match {
      case None       => astForAnonymousClass(stmt)
      case Some(name) => astForNamedClass(stmt, name)
    }
  }

  private def astForGotoStmt(stmt: PhpGotoStmt): Ast = {
    val label = stmt.label.name
    val code  = s"goto $label"

    val gotoNode = NewControlStructure()
      .controlStructureType(ControlStructureTypes.GOTO)
      .code(code)
      .lineNumber(line(stmt))

    val jumpLabel = NewJumpLabel()
      .name(label)
      .code(label)
      .lineNumber(line(stmt))

    controlStructureAst(gotoNode, condition = None, children = Ast(jumpLabel) :: Nil)
  }

  private def astForLabelStmt(stmt: PhpLabelStmt): Ast = {
    val label = stmt.label.name

    val jumpTarget = NewJumpTarget()
      .name(label)
      .code(label)
      .lineNumber(line(stmt))

    Ast(jumpTarget)
  }

  private def astForNamespaceStmt(stmt: PhpNamespaceStmt): Ast = {
    val name     = stmt.name.map(_.name).getOrElse(NameConstants.Unknown)
    val fullName = s"$filename:$name"

    val namespaceBlock = NewNamespaceBlock()
      .name(name)
      .fullName(fullName)

    scope.pushNewScope(namespaceBlock)
    val bodyStmts = stmt.stmts.map(astForStmt)
    scope.popScope()

    Ast(namespaceBlock).withChildren(bodyStmts)
  }

  private def astForHaltCompilerStmt(stmt: PhpHaltCompilerStmt): Ast = {
    val callNode = NewCall()
      .name(NameConstants.HaltCompiler)
      .methodFullName(NameConstants.HaltCompiler)
      .code(s"${NameConstants.HaltCompiler}()")
      .dispatchType(DispatchTypes.STATIC_DISPATCH)
      .typeFullName(TypeConstants.Void)
      .lineNumber(line(stmt))

    Ast(callNode)
  }

  private def astForAnonymousClass(stmt: PhpClassStmt): Ast = {
    // TODO
    Ast()
  }

  def codeForClassStmt(stmt: PhpClassStmt, name: PhpNameExpr): String = {
    // TODO Extend for anonymous classes
    val extendsString = stmt.extendsClass.map(ext => s" extends ${ext.name}").getOrElse("")
    val implementsString =
      if (stmt.implementedInterfaces.isEmpty)
        ""
      else
        s" implements ${stmt.implementedInterfaces.map(_.name).mkString(", ")}"

    s"class ${name.name}$extendsString$implementsString"
  }

  private def astForNamedClass(stmt: PhpClassStmt, name: PhpNameExpr): Ast = {
    val inheritsFrom = (stmt.extendsClass.toList ++ stmt.implementedInterfaces).map(_.name)
    val code         = codeForClassStmt(stmt, name)

    val namespacePrefix = scope.getEnclosingNamespaceName.map(_ + ".").getOrElse("")
    val fullName        = s"$namespacePrefix${name.name}"

    val typeDeclNode = NewTypeDecl()
      .name(name.name)
      .fullName(fullName)
      .code(code)
      .inheritsFromTypeFullName(inheritsFrom)
      .filename(filename)
      .lineNumber(line(stmt))

    scope.pushNewScope(typeDeclNode)
    val bodyStmts = astsForClassBody(stmt.stmts)
    val modifiers = stmt.modifiers.map(modifierNode).map(Ast(_))
    scope.popScope()

    Ast(typeDeclNode).withChildren(modifiers).withChildren(bodyStmts)
  }

  private def astForStaticAndConstInits: Option[Ast] = {
    scope.getConstAndStaticInits match {
      case Nil => None

      case inits =>
        val namespacePrefix = getNamespacePrefixForName
        val signature       = s"${TypeConstants.Void}()"
        val fullName        = s"$namespacePrefix${Defines.StaticInitMethodName}:$signature"
        val ast             = staticInitMethodAst(inits, fullName, Some(signature), TypeConstants.Void)
        Some(ast)
    }

  }

  private def astsForClassBody(bodyStmts: List[PhpStmt], createDefaultConstructor: Boolean = true): List[Ast] = {
    val classConsts = bodyStmts.collect { case cs: PhpConstStmt => cs }.flatMap(astsForConstStmt)
    val properties  = bodyStmts.collect { case cp: PhpPropertyStmt => cp }.flatMap(astsForPropertyStmt)

    val clinitAst = astForStaticAndConstInits

    val constructorDecl = bodyStmts.collectFirst {
      case m: PhpMethodDecl if m.name.name == Defines.ConstructorMethodName => m
    }

    val constructorAst = astForConstructor(constructorDecl, createDefaultConstructor).toList

    val otherBodyStmts = bodyStmts.flatMap {
      case _: PhpConstStmt => None // Handled above

      case _: PhpPropertyStmt => None // Handled above

      case method: PhpMethodDecl if method.name.name == Defines.ConstructorMethodName => None // Handled above

      case method: PhpMethodDecl =>
        Some(astForMethodDecl(method))

      case classStmt: PhpClassStmt =>
        Some(astForClassStmt(classStmt))

      case other =>
        logger.warn(s"Found unhandled class body stmt $other")
        Some(astForStmt(other))
    }

    List(classConsts, properties, clinitAst, constructorAst, otherBodyStmts).flatten
  }

  private def astForConstructor(maybeDecl: Option[PhpMethodDecl], createDefaultConstructor: Boolean): Option[Ast] = {
    maybeDecl match {
      case None if createDefaultConstructor => Some(defaultConstructorAst())

      case Some(constructorDecl) =>
        val fieldInits = scope.getFieldInits
        Some(astForMethodDecl(constructorDecl, fieldInits))

      case _ => None
    }
  }

  private def getNamespacePrefixForName: String = {
    scope.getEnclosingTypeDeclType.map(typeName => s"$typeName.").getOrElse("")
  }

  private def defaultConstructorAst(): Ast = {
    val namespacePrefix = getNamespacePrefixForName

    val signature = s"${Defines.UnresolvedSignature}(0)"
    val fullName  = s"$namespacePrefix${Defines.ConstructorMethodName}:$signature"

    val modifiers = List(ModifierTypes.VIRTUAL, ModifierTypes.PUBLIC, ModifierTypes.CONSTRUCTOR).map(modifierNode)

    val thisParam = thisParamAstForMethod(lineNumber = None)

    val methodNode = NewMethod()
      .name(Defines.ConstructorMethodName)
      .fullName(fullName)
      .signature(signature)
      .isExternal(false)
      .code(fullName)

    val methodBody = blockAst(NewBlock(), scope.getFieldInits)

    val methodReturn = NewMethodReturn().typeFullName(TypeConstants.Any)

    methodAstWithAnnotations(methodNode, thisParam :: Nil, methodBody, methodReturn, modifiers)
  }

  private def astForMemberAssignment(memberNode: NewMember, valueExpr: PhpExpr, isField: Boolean): Ast = {
    val targetAst = if (isField) {
      val code            = "$this->" + memberNode.name
      val fieldAccessNode = operatorCallNode(Operators.fieldAccess, code, line = memberNode.lineNumber)
      val identifier      = thisIdentifier(memberNode.lineNumber)
      val thisParam       = scope.lookupVariable(NameConstants.This)
      val fieldIdentifier = fieldIdentifierNode(memberNode.name, memberNode.lineNumber)
      callAst(fieldAccessNode, List(identifier, fieldIdentifier).map(Ast(_))).withRefEdges(identifier, thisParam.toList)
    } else {
      val identifierCode = memberNode.code.replaceAll("const ", "")
      val identifier = identifierNode(memberNode.name, Some(memberNode.typeFullName), line = memberNode.lineNumber)
        .code(identifierCode)
      Ast(identifier).withRefEdge(identifier, memberNode)
    }
    val value = astForExpr(valueExpr)

    val assignmentCode = s"${rootCode(targetAst)} = ${rootCode(value)}"
    val callNode       = operatorCallNode(Operators.assignment, assignmentCode, line = memberNode.lineNumber)

    callAst(callNode, List(targetAst, value))
  }

  private def astsForConstStmt(stmt: PhpConstStmt): List[Ast] = {
    stmt.consts.map { constDecl =>
      val finalModifier = Ast(modifierNode(ModifierTypes.FINAL))
      // `final const` is not allowed, so this is a safe way to represent constants in the CPG
      val modifierAsts = finalModifier :: stmt.modifiers.map(modifierNode).map(Ast(_))

      val name      = constDecl.name.name
      val code      = s"const $name"
      val someValue = Some(constDecl.value)
      astForConstOrFieldValue(name, code, someValue, line(stmt), scope.addConstOrStaticInitToScope, isField = false)
        .withChildren(modifierAsts)
    }
  }

  private def astsForPropertyStmt(stmt: PhpPropertyStmt): List[Ast] = {
    stmt.variables.map { varDecl =>
      val modifierAsts = stmt.modifiers.map(modifierNode).map(Ast(_))

      val name = varDecl.name.name
      astForConstOrFieldValue(
        name,
        "$" + name,
        varDecl.defaultValue,
        line(stmt),
        scope.addFieldInitToScope,
        isField = true
      ).withChildren(modifierAsts)
    }
  }

  private def astForConstOrFieldValue(
    name: String,
    code: String,
    value: Option[PhpExpr],
    lineNumber: Option[Integer],
    addToScope: Ast => Unit,
    isField: Boolean
  ): Ast = {
    val memberNode = NewMember()
      .name(name)
      .code(code)
      .typeFullName(TypeConstants.Any) // TODO attempt to infer this for at least primitives
      .lineNumber(lineNumber)

    value match {
      case Some(value) =>
        val assignAst = astForMemberAssignment(memberNode, value, isField)
        addToScope(assignAst)

      case None => // Nothing to do here
    }

    Ast(memberNode)
  }

  private def astForCatchStmt(stmt: PhpCatchStmt): Ast = {
    // TODO Add variable at some point. Current implementation is consistent with C++.
    stmtBlockAst(stmt.stmts, line(stmt))
  }

  private def astsForSwitchCase(caseStmt: PhpCaseStmt): List[Ast] = {
    val maybeConditionAst = caseStmt.condition.map(astForExpr)
    val jumpTarget = maybeConditionAst match {
      case Some(conditionAst) => NewJumpTarget().name("case").code(s"case ${rootCode(conditionAst)}")
      case None               => NewJumpTarget().name("default").code("default")
    }
    jumpTarget.lineNumber(line(caseStmt))

    val stmtAsts = caseStmt.stmts.map(astForStmt)

    Ast(jumpTarget) :: stmtAsts
  }

  private def codeForMethodCall(call: PhpCallExpr, targetAst: Ast, name: String): String = {
    val callOperator = if (call.isNullSafe) "?->" else "->"
    s"${rootCode(targetAst)}$callOperator$name"
  }

  private def codeForStaticMethodCall(call: PhpCallExpr, name: String): String = {
    val className =
      call.target.map(astForExpr).map(rootCode(_, Defines.UnresolvedNamespace)).getOrElse(Defines.UnresolvedNamespace)
    s"$className::$name"
  }

  private def astForCall(call: PhpCallExpr): Ast = {
    val arguments = call.args.map(astForCallArg)

    val isMethodCall = call.target.isDefined && !call.isStatic

    val targetAst = Option.when(isMethodCall)(call.target.map(astForExpr)).flatten

    val nameAst = Option.unless(call.methodName.isInstanceOf[PhpNameExpr])(astForExpr(call.methodName))
    val name =
      nameAst
        .map(rootCode(_))
        .getOrElse(call.methodName match {
          case nameExpr: PhpNameExpr => nameExpr.name
          case other =>
            logger.error(s"Found unexpected call target type: Crash for now to handle properly later: $other")
            ???
        })

    val argsCode = arguments.map(rootCode(_)).mkString(",")

    val codePrefix =
      if (isMethodCall && targetAst.isDefined)
        codeForMethodCall(call, targetAst.get, name)
      else if (call.isStatic)
        codeForStaticMethodCall(call, name)
      else
        name

    val code = s"$codePrefix($argsCode)"

    val dispatchType =
      if (call.isStatic || call.target.isEmpty)
        DispatchTypes.STATIC_DISPATCH
      else
        DispatchTypes.DYNAMIC_DISPATCH

    val fullName = call.target match {
      // Static method call with a known class name
      case Some(nameExpr: PhpNameExpr) if call.isStatic =>
        s"${nameExpr.name}.$name:${Defines.UnresolvedSignature}(${arguments.size})"

      // Function call
      case None =>
        val namespaceName = scope.getEnclosingNamespaceName.getOrElse(Defines.UnresolvedNamespace)
        s"$namespaceName.$name:${Defines.UnresolvedSignature}(${arguments.size})"

      // Other method calls. Need more type info for these.
      case _ => PropertyDefaults.MethodFullName
    }

    val callNode = NewCall()
      .name(name)
      .methodFullName(fullName)
      .code(code)
      .dispatchType(dispatchType)
      .lineNumber(line(call))

    val receiverAst = (targetAst, nameAst) match {
      case (Some(target), Some(name)) =>
        val fieldAccess = operatorCallNode(Operators.fieldAccess, codePrefix, line = line(call))
        Some(callAst(fieldAccess, target :: name :: Nil))

      case (Some(target), None) => Some(target)

      case (None, Some(name)) => Some(name)

      case (None, None) => None
    }

    callAst(callNode, arguments, receiver = receiverAst)
  }

  private def astForCallArg(arg: PhpArgument): Ast = {
    arg match {
      case PhpArg(expr, _, _, _, _) =>
        astForExpr(expr)

      case _: PhpVariadicPlaceholder =>
        val identifier =
          NewIdentifier()
            .name("...")
            .lineNumber(line(arg))
            .code("...")
            .typeFullName("PhpVariadicPlaceholder")
        Ast(identifier)
    }
  }

  private def astForVariableExpr(variable: PhpVariable): Ast = {
    // TODO Need to figure out variable variables. Maybe represent as some kind of call?
    val valueAst = astForExpr(variable.value)

    valueAst.root.collect { case root: ExpressionNew =>
      root.code = "$" + root.code
    }

    valueAst.root.collect { case root: NewIdentifier =>
      root.lineNumber = line(variable)
    }

    valueAst
  }

  private def astForNameExpr(expr: PhpNameExpr): Ast = {
    val identifier = NewIdentifier()
      .name(expr.name)
      .code(expr.name)
      .lineNumber(line(expr))

    scope.lookupVariable(identifier.name) match {
      case Some(declaringNode) =>
        diffGraph.addEdge(identifier, declaringNode, EdgeTypes.REF)

      case None =>
        // With variable variables, it's possible to use a valid variable without having an obvious assignment to it.
        // If a name is unknown at this point, assume it's a local that had a value assigned in some way at some point.
        val local = NewLocal().name(identifier.name).code("$" + identifier.code).typeFullName(identifier.typeFullName)
        scope.addToScope(local.name, local)
        diffGraph.addEdge(identifier, local, EdgeTypes.REF)
    }

    Ast(identifier)
  }

  private def astForAssignment(assignment: PhpAssignment): Ast = {
    val operatorName = assignment.assignOp

    val targetAst = astForExpr(assignment.target)
    val sourceAst = astForExpr(assignment.source)

    // TODO Handle ref assigns properly (if needed).
    val refSymbol = if (assignment.isRefAssign) "&" else ""
    val symbol    = operatorSymbols.getOrElse(assignment.assignOp, assignment.assignOp)
    val code      = s"${rootCode(targetAst)} $symbol $refSymbol${rootCode(sourceAst)}"

    val callNode = operatorCallNode(operatorName, code, line = line(assignment))
    callAst(callNode, List(targetAst, sourceAst))
  }

  private def astForScalar(scalar: PhpScalar): Ast = {
    scalar match {
      case PhpString(value, _) =>
        Ast(NewLiteral().code(value).typeFullName(TypeConstants.String).lineNumber(line(scalar)))
      case PhpInt(value, _) =>
        Ast(NewLiteral().code(value).typeFullName(TypeConstants.Int).lineNumber(line(scalar)))
      case PhpFloat(value, _) =>
        Ast(NewLiteral().code(value).typeFullName(TypeConstants.Float).lineNumber(line(scalar)))
      case PhpEncapsed(parts, _) =>
        val callNode =
          operatorCallNode(PhpBuiltins.encaps, code = /* TODO */ PhpBuiltins.encaps, line = line(scalar))
        val args = parts.map(astForExpr)
        callAst(callNode, args)
      case PhpEncapsedPart(value, _) =>
        Ast(NewLiteral().code(value).typeFullName(TypeConstants.String).lineNumber(line(scalar)))
      case null =>
        logger.warn("scalar was null")
        ???
    }
  }

  private def astForBinOp(binOp: PhpBinaryOp): Ast = {
    val leftAst  = astForExpr(binOp.left)
    val rightAst = astForExpr(binOp.right)

    val symbol = operatorSymbols.getOrElse(binOp.operator, binOp.operator)
    val code   = s"${rootCode(leftAst)} $symbol ${rootCode(rightAst)}"

    val callNode = operatorCallNode(binOp.operator, code, line = line(binOp))

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
        s"${rootCode(exprAst)}$symbol"
      else
        s"$symbol${rootCode(exprAst)}"

    val callNode = operatorCallNode(unaryOp.operator, code, line = line(unaryOp))

    callAst(callNode, exprAst :: Nil)
  }

  // TODO Move to x2cpg
  private def rootCode(ast: Ast, default: String = ""): String = {
    ast.root.flatMap(_.properties.get(PropertyNames.CODE).map(_.toString)).getOrElse(default)
  }

  private def rootType(ast: Ast): Option[String] = {
    ast.root.flatMap(_.properties.get(PropertyNames.TYPE_FULL_NAME).map(_.toString))
  }

  private def astForCastExpr(castExpr: PhpCast): Ast = {
    val typ = NewTypeRef()
      .typeFullName(registerType(castExpr.typ))
      .code(castExpr.typ)
      .lineNumber(line(castExpr))

    val expr    = astForExpr(castExpr.expr)
    val codeStr = s"(${castExpr.typ}) ${rootCode(expr)}"

    val callNode = operatorCallNode(name = Operators.cast, codeStr, Some(castExpr.typ), line(castExpr))

    callAst(callNode, Ast(typ) :: expr :: Nil)
  }

  private def astForIssetExpr(issetExpr: PhpIsset): Ast = {
    val args = issetExpr.vars.map(astForExpr)
    val code = s"${PhpBuiltins.issetFunc}(${args.map(rootCode(_).mkString(","))})"

    val callNode =
      operatorCallNode(PhpBuiltins.issetFunc, code, typeFullName = Some(TypeConstants.Bool), line = line(issetExpr))

    callAst(callNode, args)
  }
  private def astForPrintExpr(printExpr: PhpPrint): Ast = {
    val arg  = astForExpr(printExpr.expr)
    val code = s"${PhpBuiltins.printFunc}(${rootCode(arg)})"

    val callNode =
      operatorCallNode(PhpBuiltins.printFunc, code, typeFullName = Some(TypeConstants.Int), line = line(printExpr))

    callAst(callNode, arg :: Nil)
  }

  private def astForTernaryOp(ternaryOp: PhpTernaryOp): Ast = {
    val conditionAst = astForExpr(ternaryOp.condition)
    val maybeThenAst = ternaryOp.thenExpr.map(astForExpr)
    val elseAst      = astForExpr(ternaryOp.elseExpr)

    val operatorName = if (maybeThenAst.isDefined) Operators.conditional else PhpBuiltins.elvisOp
    val code = maybeThenAst match {
      case Some(thenAst) => s"${rootCode(conditionAst)} ? ${rootCode(thenAst)} : ${rootCode(elseAst)}"
      case None          => s"${rootCode(conditionAst)} ?: ${rootCode(elseAst)}"
    }

    val callNode = operatorCallNode(operatorName, code, line = line(ternaryOp))

    val args = List(Some(conditionAst), maybeThenAst, Some(elseAst)).flatten
    callAst(callNode, args)
  }

  private def astForThrow(expr: PhpThrowExpr): Ast = {
    val thrownExpr = astForExpr(expr.expr)
    val code       = s"throw ${rootCode(thrownExpr)}"

    val throwNode =
      NewControlStructure()
        .controlStructureType(ControlStructureTypes.THROW)
        .code(code)
        .lineNumber(line(expr))

    Ast(throwNode).withChild(thrownExpr)
  }

  private def astForClone(expr: PhpCloneExpr): Ast = {
    val argAst  = astForExpr(expr.expr)
    val argType = rootType(argAst)
    val code    = s"clone ${rootCode(argAst)}"

    val callNode = operatorCallNode(PhpBuiltins.cloneFunc, code, argType, line(expr))

    callAst(callNode, argAst :: Nil)
  }

  private def astForEmpty(expr: PhpEmptyExpr): Ast = {
    val argAst = astForExpr(expr.expr)
    val code   = s"empty(${rootCode(argAst)})"

    val callNode =
      operatorCallNode(PhpBuiltins.emptyFunc, code, typeFullName = Some(TypeConstants.Bool), line = line(expr))

    callAst(callNode, argAst :: Nil)
  }

  private def astForEval(expr: PhpEvalExpr): Ast = {
    val argAst = astForExpr(expr.expr)
    val code   = s"eval(${rootCode(argAst)})"

    val callNode =
      operatorCallNode(PhpBuiltins.evalFunc, code, typeFullName = Some(TypeConstants.Bool), line = line(expr))

    callAst(callNode, argAst :: Nil)
  }

  private def astForExit(expr: PhpExitExpr): Ast = {
    val args = expr.expr.map(astForExpr)
    val code = s"exit(${args.map(rootCode(_)).getOrElse("")})"

    val callNode = operatorCallNode(PhpBuiltins.exitFunc, code, Some(TypeConstants.Void), line(expr))

    callAst(callNode, args.toList)
  }

  private def getTmpLocal(typeFullName: Option[String], lineNumber: Option[Integer]): NewLocal = {
    val name = getNewTmpName

    val local = NewLocal()
      .name(name)
      .code("$" + name)
      .lineNumber(lineNumber)

    typeFullName.foreach(local.typeFullName(_))

    local
  }

  private def identifierAstFromLocal(local: NewLocal, lineNumber: Option[Integer] = None): Ast = {
    val identifier = identifierNode(local.name, typeFullName = Some(local.typeFullName), lineNumber)
      .code("$" + local.name)
    Ast(identifier).withRefEdge(identifier, local)
  }

  private def astForArrayExpr(expr: PhpArrayExpr): Ast = {
    val idxTracker = new ArrayIndexTracker

    val tmpLocal = getTmpLocal(Some(TypeConstants.Array), line(expr))
    scope.pushNewScope(tmpLocal)

    val itemAssignments = expr.items.map(assignForArrayItem(_, tmpLocal, idxTracker))
    val arrayBlock      = NewBlock().lineNumber(line(expr))

    Ast(arrayBlock)
      .withChild(Ast(tmpLocal))
      .withChildren(itemAssignments)
      .withChild(identifierAstFromLocal(tmpLocal))
  }

  private def dimensionFromSimpleScalar(scalar: PhpSimpleScalar, idxTracker: ArrayIndexTracker): PhpExpr = {
    val maybeIntValue = scalar match {
      case string: PhpString =>
        string.value
          .drop(1)
          .dropRight(1)
          .toIntOption

      case number => number.value.toIntOption
    }

    maybeIntValue match {
      case Some(intValue) =>
        idxTracker.updateValue(intValue)
        PhpInt(intValue.toString, scalar.attributes)

      case None =>
        scalar
    }
  }
  private def assignForArrayItem(item: PhpArrayItem, arrayLocal: NewLocal, idxTracker: ArrayIndexTracker): Ast = {
    // It's perhaps a bit clumsy to reconstruct PhpExpr nodes here, but reuse astForArrayDimExpr for consistency
    val variable = PhpVariable(PhpNameExpr(arrayLocal.name, item.attributes), item.attributes)

    val dimension = item.key match {
      case Some(key: PhpSimpleScalar) => dimensionFromSimpleScalar(key, idxTracker)
      case Some(key)                  => key
      case None                       => PhpInt(idxTracker.next, item.attributes)
    }

    val dimFetchNode = PhpArrayDimFetchExpr(variable, Some(dimension), item.attributes)
    val dimFetchAst  = astForArrayDimFetchExpr(dimFetchNode)

    val valueAst = astForArrayItemValue(item)

    val assignCode = s"${rootCode(dimFetchAst)} = ${rootCode(valueAst)}"

    val assignNode = operatorCallNode(Operators.assignment, assignCode, line = line(item))

    callAst(assignNode, dimFetchAst :: valueAst :: Nil)
  }

  private def astForArrayItemValue(item: PhpArrayItem): Ast = {
    val exprAst   = astForExpr(item.value)
    val valueCode = rootCode(exprAst)

    if (item.byRef) {
      val parentCall = operatorCallNode(Operators.addressOf, s"&$valueCode", line = line(item))
      callAst(parentCall, exprAst :: Nil)
    } else if (item.unpack) {
      val parentCall = operatorCallNode(PhpBuiltins.unpack, s"...$valueCode", line = line(item))
      callAst(parentCall, exprAst :: Nil)
    } else {
      exprAst
    }
  }

  private def astForArrayDimFetchExpr(expr: PhpArrayDimFetchExpr): Ast = {
    val variableAst  = astForExpr(expr.variable)
    val variableCode = rootCode(variableAst)

    expr.dimension match {
      case Some(dimension) =>
        val dimensionAst = astForExpr(dimension)
        val code         = s"$variableCode[${rootCode(dimensionAst)}]"
        val accessNode   = operatorCallNode(Operators.indexAccess, code, line = line(expr))
        callAst(accessNode, variableAst :: dimensionAst :: Nil)

      case None =>
        val accessNode = operatorCallNode(PhpBuiltins.emptyArrayIdx, s"$variableCode[]", line = line(expr))
        callAst(accessNode, variableAst :: Nil)
    }
  }

  private def astForErrorSuppressExpr(expr: PhpErrorSuppressExpr): Ast = {
    val childAst = astForExpr(expr.expr)

    val code         = s"@${rootCode(childAst)}"
    val suppressNode = operatorCallNode(PhpBuiltins.errorSuppress, code, line = line(expr))
    rootType(childAst).foreach(suppressNode.typeFullName(_))

    callAst(suppressNode, childAst :: Nil)
  }

  private def astForInstanceOfExpr(expr: PhpInstanceOfExpr): Ast = {
    val exprAst  = astForExpr(expr.expr)
    val classAst = astForExpr(expr.className)

    val code           = s"${rootCode(exprAst)} instanceof ${rootCode(classAst)}"
    val instanceOfNode = operatorCallNode(Operators.instanceOf, code, Some(TypeConstants.Bool), line(expr))

    callAst(instanceOfNode, exprAst :: classAst :: Nil)
  }

  private def astForPropertyFetchExpr(expr: PhpPropertyFetchExpr): Ast = {
    val objExprAst = astForExpr(expr.expr)

    val fieldAst = expr.name match {
      case name: PhpNameExpr => Ast(fieldIdentifierNode(name.name, line(expr)))
      case other             => astForExpr(other)
    }

    val accessSymbol =
      if (expr.isStatic)
        "::"
      else if (expr.isNullsafe)
        "?->"
      else
        "->"

    val code            = s"${rootCode(objExprAst)}$accessSymbol${rootCode(fieldAst)}"
    val fieldAccessNode = operatorCallNode(Operators.fieldAccess, code, line = line(expr))

    callAst(fieldAccessNode, objExprAst :: fieldAst :: Nil)
  }

  private def astForClassConstFetchExpr(expr: PhpClassConstFetchExpr): Ast = {
    val target              = astForExpr(expr.className)
    val fieldIdentifierName = expr.constantName.map(_.name).getOrElse(NameConstants.Unknown)

    val fieldIdentifier = fieldIdentifierNode(fieldIdentifierName, line(expr))

    val fieldAccessCode = s"${rootCode(target)}::${fieldIdentifier.code}"

    val fieldAccessCall = operatorCallNode(Operators.fieldAccess, fieldAccessCode, line = line(expr))

    callAst(fieldAccessCall, List(target, Ast(fieldIdentifier)))
  }

  private def line(phpNode: PhpNode): Option[Integer] = phpNode.attributes.lineNumber
}

object AstCreator {
  object TypeConstants {
    val String: String = "string"
    val Int: String    = "int"
    val Float: String  = "float"
    val Bool: String   = "bool"
    val Void: String   = "void"
    val Any: String    = "ANY"
    val Array: String  = "array"
  }

  object NameConstants {
    val HaltCompiler: String = "__halt_compiler"
    val This: String         = "this"
    val Unknown: String      = "UNKNOWN"
  }

  val operatorSymbols: Map[String, String] = Map(
    Operators.and                            -> "&",
    Operators.or                             -> "|",
    Operators.xor                            -> "^",
    Operators.logicalAnd                     -> "&&",
    Operators.logicalOr                      -> "||",
    PhpBuiltins.coalesceOp                   -> "??",
    PhpBuiltins.concatOp                     -> ".",
    Operators.division                       -> "/",
    Operators.equals                         -> "==",
    Operators.greaterEqualsThan              -> ">=",
    Operators.greaterThan                    -> ">",
    PhpBuiltins.identicalOp                  -> "===",
    PhpBuiltins.logicalXorOp                 -> "xor",
    Operators.minus                          -> "-",
    Operators.modulo                         -> "%",
    Operators.multiplication                 -> "*",
    Operators.notEquals                      -> "!=",
    PhpBuiltins.notIdenticalOp               -> "!==",
    Operators.plus                           -> "+",
    Operators.exponentiation                 -> "**",
    Operators.shiftLeft                      -> "<<",
    Operators.arithmeticShiftRight           -> ">>",
    Operators.lessEqualsThan                 -> "<=",
    Operators.lessThan                       -> "<",
    PhpBuiltins.spaceshipOp                  -> "<=>",
    Operators.not                            -> "~",
    Operators.logicalNot                     -> "!",
    Operators.postDecrement                  -> "--",
    Operators.postIncrement                  -> "++",
    Operators.preDecrement                   -> "--",
    Operators.preIncrement                   -> "++",
    Operators.minus                          -> "-",
    Operators.plus                           -> "+",
    Operators.assignment                     -> "=",
    Operators.assignmentAnd                  -> "&=",
    Operators.assignmentOr                   -> "|=",
    Operators.assignmentXor                  -> "^=",
    PhpBuiltins.assignmentCoalesceOp         -> "??=",
    PhpBuiltins.assignmentConcatOp           -> ".=",
    Operators.assignmentDivision             -> "/=",
    Operators.assignmentMinus                -> "-=",
    Operators.assignmentModulo               -> "%=",
    Operators.assignmentMultiplication       -> "*=",
    Operators.assignmentPlus                 -> "+=",
    Operators.assignmentExponentiation       -> "**=",
    Operators.assignmentShiftLeft            -> "<<=",
    Operators.assignmentArithmeticShiftRight -> ">>="
  )

}
