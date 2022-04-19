package io.joern.jssrc2cpg.astcreation

import io.joern.jssrc2cpg.datastructures.Stack._
import io.joern.jssrc2cpg.parser.BabelAst
import io.joern.jssrc2cpg.parser.BabelJsonParser.ParseResult
import io.joern.jssrc2cpg.Config
import io.joern.jssrc2cpg.datastructures.scope.BlockScopeElement
import io.joern.jssrc2cpg.datastructures.scope.MethodScope
import io.joern.jssrc2cpg.datastructures.scope.MethodScopeElement
import io.joern.jssrc2cpg.datastructures.scope.ResolvedReference
import io.joern.jssrc2cpg.datastructures.scope.Scope
import io.joern.jssrc2cpg.datastructures.scope.ScopeType
import io.joern.jssrc2cpg.passes.Defines
import io.joern.jssrc2cpg.passes.GlobalBuiltins
import AstCreatorHelper.OptionSafeAst
import io.joern.jssrc2cpg.datastructures.scope.BlockScope
import io.joern.jssrc2cpg.datastructures.scope.ScopeElement
import io.joern.jssrc2cpg.datastructures.scope.ScopeElementIterator
import io.joern.jssrc2cpg.parser.BabelNodeInfo
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.codepropertygraph.generated.{EvaluationStrategies, NodeTypes}
import overflowdb.BatchedUpdate.DiffGraphBuilder
import io.joern.x2cpg.Ast
import io.joern.x2cpg.datastructures.Global
import io.joern.x2cpg.AstCreatorBase
import io.shiftleft.codepropertygraph.generated.DispatchTypes
import io.shiftleft.codepropertygraph.generated.EdgeTypes
import io.shiftleft.codepropertygraph.generated.ModifierTypes
import io.shiftleft.codepropertygraph.generated.Operators
import org.slf4j.{Logger, LoggerFactory}
import ujson.Obj
import ujson.Value

import scala.collection.mutable
import scala.util.Try

class AstCreator(val config: Config, val parserResult: ParseResult, val global: Global)
    extends AstCreatorBase(parserResult.fullPath)
    with AstNodeBuilder
    with AstCreatorHelper {

  protected val logger: Logger = LoggerFactory.getLogger(classOf[AstCreator])

  protected val scope = new Scope()

  // TypeDecls with their bindings (with their refs) for lambdas and methods are not put in the AST
  // where the respective nodes are defined. Instead we put them under the parent TYPE_DECL in which they are defined.
  // To achieve this we need this extra stack.
  private val methodAstParentStack: Stack[NewNode]              = new Stack[NewNode]()
  private val localAstParentStack: Stack[NewBlock]              = new Stack[NewBlock]()
  private val usedVariableNames: mutable.HashMap[String, Int]   = mutable.HashMap.empty[String, Int]
  private val functionNodeToNameAndFullName                     = mutable.HashMap.empty[BabelNodeInfo, (String, String)]
  private val functionFullNames                                 = mutable.HashSet.empty[String]
  private val metaTypeRefIdStack                                = mutable.Stack.empty[NewTypeRef]
  private val typeFullNameToPostfix                             = mutable.HashMap.empty[String, Int]
  private val typeToNameAndFullName                             = mutable.HashMap.empty[BabelNodeInfo, (String, String)]
  protected val dynamicInstanceTypeStack: mutable.Stack[String] = mutable.Stack.empty[String]

  override def createAst(): DiffGraphBuilder = {
    val namespaceBlock = globalNamespaceBlock()
    methodAstParentStack.push(namespaceBlock)
    val ast = Ast(namespaceBlock).withChild(createProgramMethod())
    Ast.storeInDiffGraph(ast, diffGraph)
    createVariableReferenceLinks()
    diffGraph
  }

  protected def astsForExpressionStatement(exprStmt: BabelNodeInfo): List[Ast] =
    astsForNode(exprStmt.json("expression"))

  protected def createBuiltinStaticCall(callExpr: BabelNodeInfo, callee: BabelNodeInfo, methodFullName: String): Ast = {
    val methodName = callee.node match {
      case BabelAst.MemberExpression =>
        code(callee.json("property"))
      case BabelAst.Identifier =>
        callee.code
      case _ => callee.code
    }
    val callNode =
      createStaticCallNode(callExpr.code, methodName, methodFullName, callee.lineNumber, callee.columnNumber)
    val args = astsForNodes(callExpr.json("arguments").arr.toList)
    callAst(callNode, args)
  }

  protected def handleCallNodeArgs(
    callExpr: BabelNodeInfo,
    receiverNode: NewNode,
    baseNode: NewNode,
    functionBaseNode: NewNode,
    functionPropertyNode: Option[NewFieldIdentifier]
  ): Ast = {
    val args = astsForNodes(callExpr.json("arguments").arr.toList)

    val baseCode = codeOf(functionBaseNode)
    val propertyCode = functionPropertyNode match {
      case Some(id) => "." + codeOf(id)
      case None     => ""
    }

    val argsCode = args.map(a => codeOf(a.nodes.head)).mkString("(", ", ", ")")
    val code     = s"$baseCode$propertyCode$argsCode"

    val callNode = createCallNode(code, "", DispatchTypes.DYNAMIC_DISPATCH, callExpr.lineNumber, callExpr.columnNumber)
    callAst(callNode, Ast(baseNode) +: args, Some(Ast(receiverNode)))
  }

  protected def astsForCallExpression(callExpr: BabelNodeInfo): List[Ast] = {
    val callee         = createBabelNodeInfo(callExpr.json("callee"))
    val methodFullName = callee.code
    val callNode = if (GlobalBuiltins.builtins.contains(methodFullName)) {
      createBuiltinStaticCall(callExpr, callee, methodFullName)
    } else {
      val (functionBaseAsts, functionPropertyNode, receiverAsts, baseNode) = callee.node match {
        case BabelAst.MemberExpression =>
          // "this" argument is coming from source.
          val base = createBabelNodeInfo(callee.json("object"))
          base.node match {
            case BabelAst.Identifier =>
              val receiverAsts = astsForNode(callee.json)
              receiverAsts.foreach(Ast.storeInDiffGraph(_, diffGraph))
              val baseNode = createIdentifierNode(base.code, base).order(1).argumentIndex(1)
              scope.addVariableReference(base.code, baseNode)
              (receiverAsts, None, receiverAsts, baseNode)
            case _ =>
              // TODO: check for used nodes
              val tmpVarName  = generateUnusedVariableName(usedVariableNames, Set.empty, "_tmp")
              val baseTmpNode = createIdentifierNode(tmpVarName, base)
              scope.addVariableReference(tmpVarName, baseTmpNode)
              val baseAsts = astsForNode(base.json)
              baseAsts.foreach(Ast.storeInDiffGraph(_, diffGraph))
              val code = s"(${codeOf(baseTmpNode)} = ${base.code})"
              val tmpAssignmentAst =
                createAssignment(baseTmpNode, baseAsts.head.nodes.head, code, base.lineNumber, base.columnNumber)
              val member     = createBabelNodeInfo(callee.json("property"))
              val memberNode = createFieldIdentifierNode(member.code, member.lineNumber, member.columnNumber)
              val fieldAccessAst =
                createFieldAccess(tmpAssignmentAst.nodes.head, memberNode, callee.lineNumber, callee.columnNumber)
              val thisTmpNode = createIdentifierNode(tmpVarName, callee)
              scope.addVariableReference(tmpVarName, thisTmpNode)

              Ast.storeInDiffGraph(tmpAssignmentAst, diffGraph)
              Ast.storeInDiffGraph(fieldAccessAst, diffGraph)

              (baseAsts, Some(memberNode), Seq(fieldAccessAst), thisTmpNode)
          }
        case _ =>
          val receiverAsts = astsForNode(callee.json)
          receiverAsts.foreach(Ast.storeInDiffGraph(_, diffGraph))
          val thisNode = createIdentifierNode("this", callee)
          scope.addVariableReference(thisNode.name, thisNode)
          (receiverAsts, None, receiverAsts, thisNode)
      }
      handleCallNodeArgs(
        callExpr,
        receiverAsts.head.nodes.head,
        baseNode,
        functionBaseAsts.head.nodes.head,
        functionPropertyNode
      )
    }
    List(callNode)
  }

  protected def astsForMemberExpression(memberExpr: BabelNodeInfo): List[Ast] = {
    val baseAsts = astsForNode(memberExpr.json("object"))
    baseAsts.foreach(Ast.storeInDiffGraph(_, diffGraph))
    val memberNode =
      createFieldIdentifierNode(code(memberExpr.json("property")), memberExpr.lineNumber, memberExpr.columnNumber)
    val accessAst =
      createFieldAccess(baseAsts.head.nodes.head, memberNode, memberExpr.lineNumber, memberExpr.columnNumber)
    List(accessAst)
  }

  protected def astsForIdentifier(ident: BabelNodeInfo): List[Ast] = {
    val name      = ident.json("name").str
    val identNode = createIdentifierNode(name, ident)
    scope.addVariableReference(name, identNode)
    List(Ast(identNode))
  }

  protected def astsForStringLiteral(stringLiteral: BabelNodeInfo): List[Ast] =
    List(
      Ast(
        createLiteralNode(
          stringLiteral.code,
          Some(Defines.STRING.label),
          stringLiteral.lineNumber,
          stringLiteral.columnNumber
        )
      )
    )

  protected def astsForNumericLiteral(numericLiteral: BabelNodeInfo): List[Ast] =
    List(
      Ast(
        createLiteralNode(
          numericLiteral.code,
          Some(Defines.NUMBER.label),
          numericLiteral.lineNumber,
          numericLiteral.columnNumber
        )
      )
    )

  protected def computeScopePath(stack: Option[ScopeElement]): String =
    new ScopeElementIterator(stack)
      .to(Seq)
      .reverse
      .collect { case methodScopeElement: MethodScopeElement =>
        methodScopeElement.name
      }
      .mkString(":")

  protected def calcMethodNameAndFullName(func: BabelNodeInfo): (String, String) = {
    def calcMethodName(func: BabelNodeInfo): String = {
      val name = func match {
        case _ if hasKey(func.json, "kind") && func.json("kind").str == "method" =>
          func.json("key")("name").str
        case _ if hasKey(func.json, "kind") && func.json("kind").str == "constructor" =>
          "<constructor>"
        case _ if func.json("id").isNull =>
          "anonymous"
        case _ =>
          func.json("id")("name").str
      }
      name
    }

    // functionNode.getName is not necessarily unique and thus the full name calculated based on the scope
    // is not necessarily unique. Specifically we have this problem with lambda functions which are defined
    // in the same scope.
    functionNodeToNameAndFullName.get(func) match {
      case Some(nameAndFullName) =>
        nameAndFullName
      case None =>
        val intendedName   = calcMethodName(func)
        val fullNamePrefix = parserResult.fullPath + ":" + computeScopePath(scope.getScopeHead) + ":"
        var name           = intendedName
        var fullName       = ""

        var isUnique = false
        var i        = 1
        while (!isUnique) {
          fullName = fullNamePrefix + name
          if (functionFullNames.contains(fullName)) {
            name = intendedName + i.toString
            i += 1
          } else {
            isUnique = true
          }
        }

        functionFullNames.add(fullName)
        functionNodeToNameAndFullName(func) = (name, fullName)
        (name, fullName)
    }
  }

  private def createMethodNode(
    func: BabelNodeInfo,
    shouldCreateFunctionReference: Boolean = false,
    shouldCreateAssignmentCall: Boolean = false
  ): (List[Ast], NewMethod) = {
    val (methodName, methodFullName) = calcMethodNameAndFullName(func)
    val methodRefNode = if (!shouldCreateFunctionReference) {
      None
    } else { Some(createMethodRefNode(methodName, methodFullName, func)) }

    val callAsts = if (shouldCreateAssignmentCall && shouldCreateFunctionReference) {
      val idNode  = createIdentifierNode(methodName, func)
      val idLocal = createLocalNode(methodName, methodFullName)
      diffGraph.addEdge(localAstParentStack.head, idLocal, EdgeTypes.AST)
      scope.addVariable(methodName, idLocal, BlockScope)
      val code       = s"$methodName = ${func.code}"
      val assignment = createAssignment(idNode, methodRefNode.get, code, func.lineNumber, func.columnNumber)
      List(assignment)
    } else {
      List.empty
    }

    val methodNode          = createMethodNode(methodName, methodFullName, func)
    val virtualModifierNode = NewModifier().modifierType(ModifierTypes.VIRTUAL)

    methodAstParentStack.push(methodNode)

    val block             = func.json("body")
    val blockLineNumber   = line(block)
    val blockColumnNumber = column(block)
    val blockCode         = code(block)
    val blockNode = NewBlock()
      .typeFullName(Defines.ANY.label)
      .code(blockCode)
      .lineNumber(blockLineNumber)
      .columnNumber(blockColumnNumber)

    val capturingRefNode =
      if (shouldCreateFunctionReference) {
        methodRefNode
      } else {
        metaTypeRefIdStack.headOption
      }
    scope.pushNewMethodScope(methodFullName, methodName, blockNode, capturingRefNode)

    val thisNode =
      createParameterInNode("this", "this", 0, isVariadic = false, line = func.lineNumber, column = func.columnNumber)

    val paramNodes = withIndex(func.json("params").arr.toSeq) { (param, index) =>
      createBabelNodeInfo(param) match {
        case rest @ BabelNodeInfo(BabelAst.RestElement) =>
          createParameterInNode(
            rest.code.replace("...", ""),
            rest.code,
            index,
            isVariadic = true,
            rest.lineNumber,
            rest.columnNumber
          )
        case other =>
          createParameterInNode(other.code, other.code, index, isVariadic = false, other.lineNumber, other.columnNumber)
      }
    }

    localAstParentStack.push(blockNode)

    val bodyStmtAsts = createBlockStatementAsts(block("body"))

    val methodReturnNode = createMethodReturnNode(func)

    localAstParentStack.pop()
    scope.popScope()
    methodAstParentStack.pop()

    val functionTypeAndTypeDeclAst =
      createFunctionTypeAndTypeDecl(
        methodNode,
        methodAstParentStack.head,
        methodName,
        methodFullName,
        parserResult.fullPath
      )

    val mAst =
      methodAst(methodNode, thisNode +: paramNodes.toList, Ast(blockNode).withChildren(bodyStmtAsts), methodReturnNode)
        .withChild(Ast(virtualModifierNode))

    Ast.storeInDiffGraph(mAst, diffGraph)
    Ast.storeInDiffGraph(functionTypeAndTypeDeclAst, diffGraph)
    diffGraph.addEdge(methodAstParentStack.head, methodNode, EdgeTypes.AST)

    (callAsts ++ methodRefNode.map(Ast(_)), methodNode)
  }

  protected def astsForFunctionDeclaration(
    func: BabelNodeInfo,
    shouldCreateFunctionReference: Boolean = false,
    shouldCreateAssignmentCall: Boolean = false
  ): List[Ast] = createMethodNode(func, shouldCreateFunctionReference, shouldCreateAssignmentCall)._1

  protected def astsForVariableDeclarator(declarator: Value, scopeType: ScopeType): List[Ast] = {
    val id   = createBabelNodeInfo(declarator("id"))
    val init = Try(createBabelNodeInfo(declarator("init"))).toOption

    val typeFullName = init match {
      case Some(f @ BabelNodeInfo(BabelAst.FunctionDeclaration)) =>
        val (_, methodFullName) = calcMethodNameAndFullName(f)
        methodFullName
      case _ => Defines.ANY.label
    }

    val localNode = createLocalNode(id.code, typeFullName)
    diffGraph.addEdge(localAstParentStack.head, localNode, EdgeTypes.AST)
    scope.addVariable(id.code, localNode, scopeType)

    init match {
      case Some(f @ BabelNodeInfo(BabelAst.FunctionDeclaration)) =>
        val destAsts   = astsForNode(id.json)
        val sourceAsts = astsForFunctionDeclaration(f, shouldCreateFunctionReference = true)
        val assigmentCallAst =
          createAssignment(
            destAsts.head.nodes.head,
            sourceAsts.head.nodes.head,
            code(declarator),
            line = line(declarator),
            column = column(declarator)
          )
        List(assigmentCallAst) ++ destAsts ++ sourceAsts
      case Some(initExpr) =>
        val destAsts   = astsForNode(id.json)
        val sourceAsts = astsForNode(initExpr.json)
        val assigmentCallAst =
          createAssignment(
            destAsts.head.nodes.head,
            sourceAsts.head.nodes.head,
            code(declarator),
            line = line(declarator),
            column = column(declarator)
          )
        List(assigmentCallAst) ++ destAsts ++ sourceAsts
      case None => List.empty
    }
  }

  protected def astsForVariableDeclaration(declaration: BabelNodeInfo): List[Ast] = {
    val scopeType = if (declaration.json("kind").str == "let") {
      BlockScope
    } else {
      MethodScope
    }
    declaration.json("declarations").arr.toList.flatMap { d =>
      astsForVariableDeclarator(d, scopeType)
    }
  }

  protected def astsForAssignmentExpression(assignment: BabelNodeInfo): List[Ast] = {
    val op = assignment.json("operator").str match {
      case "="    => Operators.assignment
      case "+="   => Operators.assignmentPlus
      case "-="   => Operators.assignmentMinus
      case "*="   => Operators.assignmentMultiplication
      case "/="   => Operators.assignmentDivision
      case "%="   => Operators.assignmentModulo
      case "**="  => Operators.assignmentExponentiation
      case "&="   => Operators.assignmentAnd
      case "&&="  => Operators.assignmentAnd
      case "|="   => Operators.assignmentOr
      case "||="  => Operators.assignmentOr
      case "^="   => Operators.assignmentXor
      case "<<="  => Operators.assignmentShiftLeft
      case ">>="  => Operators.assignmentArithmeticShiftRight
      case ">>>=" => Operators.assignmentLogicalShiftRight
      case "??="  => Operators.notNullAssert
      case other =>
        logger.warn(s"Unknown assignment operator: '$other'")
        Operators.assignment
    }

    val lhsAsts = astsForNode(assignment.json("left"))
    val rhsAsts = astsForNode(assignment.json("right"))

    val callNode =
      createCallNode(assignment.code, op, DispatchTypes.STATIC_DISPATCH, assignment.lineNumber, assignment.columnNumber)

    List(callAst(callNode, lhsAsts ++ rhsAsts))
  }

  protected def createBlockStatementAsts(json: Value): List[Ast] = {
    val blockStmts = json.arr.map(createBabelNodeInfo).sortBy(_.node != BabelAst.FunctionDeclaration).toList
    blockStmts.flatMap {
      case func @ BabelNodeInfo(BabelAst.FunctionDeclaration) =>
        astsForFunctionDeclaration(func, shouldCreateAssignmentCall = true, shouldCreateFunctionReference = true)
      case other => astsForNode(other.json)
    }
  }

  protected def astsForBlockStatement(block: BabelNodeInfo): List[Ast] = {
    val blockNode = createBlockNode(block.code, block.lineNumber, block.columnNumber)
    scope.pushNewBlockScope(blockNode)
    localAstParentStack.push(blockNode)
    val blockStatementAsts = createBlockStatementAsts(block.json("body"))
    localAstParentStack.pop()
    scope.popScope()
    List(Ast(blockNode).withChildren(blockStatementAsts))
  }

  protected def astsForBinaryExpression(binExpr: BabelNodeInfo): List[Ast] = {
    val op = binExpr.json("operator").str match {
      case "+"          => Operators.addition
      case "-"          => Operators.subtraction
      case "/"          => Operators.division
      case "%"          => Operators.modulo
      case "*"          => Operators.multiplication
      case "**"         => Operators.exponentiation
      case "&"          => Operators.and
      case ">>"         => Operators.arithmeticShiftRight
      case ">>>"        => Operators.arithmeticShiftRight
      case "<<"         => Operators.shiftLeft
      case "^"          => Operators.xor
      case "=="         => Operators.equals
      case "==="        => Operators.equals
      case "!="         => Operators.notEquals
      case "!=="        => Operators.notEquals
      case "in"         => Operators.in
      case ">"          => Operators.greaterThan
      case "<"          => Operators.lessThan
      case ">="         => Operators.greaterEqualsThan
      case "<="         => Operators.lessEqualsThan
      case "instanceof" => Operators.instanceOf
      case "case"       => "<operator>.case"
      case other =>
        logger.warn(s"Unknown binary operator: '$other'")
        Operators.assignment
    }

    val lhsAsts = astsForNode(binExpr.json("left"))
    val rhsAsts = astsForNode(binExpr.json("right"))

    val callNode =
      createCallNode(binExpr.code, op, DispatchTypes.STATIC_DISPATCH, binExpr.lineNumber, binExpr.columnNumber)
    List(callAst(callNode, lhsAsts ++ rhsAsts))
  }

  protected def astsForUpdateExpression(updateExpr: BabelNodeInfo): List[Ast] = {
    val op = updateExpr.json("operator").str match {
      case "++" => Operators.preIncrement
      case "--" => Operators.preDecrement
      case other =>
        logger.warn(s"Unknown update operator: '$other'")
        Operators.assignment
    }

    val argumentAsts = astsForNode(updateExpr.json("argument"))

    val callNode =
      createCallNode(updateExpr.code, op, DispatchTypes.STATIC_DISPATCH, updateExpr.lineNumber, updateExpr.columnNumber)
    List(callAst(callNode, argumentAsts))
  }

  protected def astsForUnaryExpression(unaryExpr: BabelNodeInfo): List[Ast] = {
    val op = unaryExpr.json("operator").str match {
      case "void"   => "<operator>.void"
      case "throw"  => "<operator>.throw"
      case "delete" => Operators.delete
      case "!"      => Operators.logicalNot
      case "+"      => Operators.plus
      case "-"      => Operators.minus
      case "~"      => "<operator>.bitNot"
      case "typeof" => Operators.instanceOf
      case other =>
        logger.warn(s"Unknown update operator: '$other'")
        Operators.assignment
    }

    val argumentAsts = astsForNode(unaryExpr.json("argument"))

    val callNode =
      createCallNode(unaryExpr.code, op, DispatchTypes.STATIC_DISPATCH, unaryExpr.lineNumber, unaryExpr.columnNumber)
    List(callAst(callNode, argumentAsts))
  }

  protected def astsForReturnStatement(ret: BabelNodeInfo): List[Ast] = {
    val retNode = createReturnNode(ret)
    safeObj(ret.json, "argument").map { argument =>
      val argAsts = astsForNode(Obj(argument))
      Ast(retNode).withChildren(argAsts).withArgEdges(retNode, argAsts)
    }.toList
  }

  protected def astsForSequenceExpression(seq: BabelNodeInfo): List[Ast] = {
    val blockNode = createBlockNode(seq.code, seq.lineNumber, seq.columnNumber)
    scope.pushNewBlockScope(blockNode)
    localAstParentStack.push(blockNode)
    val sequenceExpressionAsts = createBlockStatementAsts(seq.json("expressions"))
    localAstParentStack.pop()
    scope.popScope()
    List(Ast(blockNode).withChildren(sequenceExpressionAsts))
  }

  protected def calcTypeNameAndFullName(classNode: BabelNodeInfo): (String, String) = {
    def calcTypeName(classNode: BabelNodeInfo): String = {
      val typeName = Try(createBabelNodeInfo(classNode.json("id")).code).toOption match {
        case Some(ident) => ident
        // in JS it is possible to create anonymous classes; hence no name
        case None =>
          "_anon_cdecl"
      }
      typeName
    }

    typeToNameAndFullName.get(classNode) match {
      case Some(nameAndFullName) =>
        nameAndFullName
      case None =>
        val name             = calcTypeName(classNode)
        val fullNamePrefix   = parserResult.fullPath + ":" + computeScopePath(scope.getScopeHead) + ":"
        val intendedFullName = fullNamePrefix + name
        val postfix          = typeFullNameToPostfix.getOrElse(intendedFullName, 0)

        val resultingFullName =
          if (postfix == 0) {
            intendedFullName
          } else {
            intendedFullName + postfix.toString
          }

        typeFullNameToPostfix.put(intendedFullName, postfix + 1)
        (name, resultingFullName)
    }

  }

  private def classMembers(clazz: BabelNodeInfo, withConstructor: Boolean = true): Seq[Value] = {
    val allMembers = Try(clazz.json("body")("body").arr).toOption.toSeq.flatten
    if (withConstructor) {
      allMembers
    } else {
      allMembers.filterNot(_("kind").str == "constructor")
    }
  }

  private def classConstructor(clazz: BabelNodeInfo): Option[Value] =
    classMembers(clazz).find(_("kind").str == "constructor")

  private def classConstructor(typeName: String, classExpr: BabelNodeInfo): NewMethod = {
    val maybeClassConstructor = classConstructor(classExpr)
    val methodNode = maybeClassConstructor match {
      case Some(classConstructor) => createMethodNode(createBabelNodeInfo(classConstructor))._2
      case _ =>
        val code = "constructor() {}"
        val fakeConstructorCode = """{
            | "type": "ClassMethod",
            | "key": {
            |   "type": "Identifier",
            |   "name": "constructor"
            | },
            | "kind": "constructor",
            | "id": null,
            | "params": [],
            | "body": {
            |   "type": "BlockStatement",
            |   "body": []
            | }
            |}""".stripMargin
        val methodNode = createMethodNode(createBabelNodeInfo(ujson.read(fakeConstructorCode)))._2
        methodNode.code(code)
    }
    val name     = methodNode.name.replace("<", s"$typeName<")
    val fullName = methodNode.fullName.replace("<", s"$typeName<")
    methodNode.name(name).fullName(fullName)
  }

  protected def astsForClass(clazz: BabelNodeInfo): List[Ast] = {
    val (typeName, typeFullName) = calcTypeNameAndFullName(clazz)
    val metaTypeName             = s"$typeName<meta>"
    val metaTypeFullName         = s"$typeFullName<meta>"

    val classTypeNode = createTypeNode(typeName, typeFullName)
    Ast.storeInDiffGraph(Ast(classTypeNode), diffGraph)

    // We do not need to look at classNode.getClassHeritage because
    // the CPG only allows us to encode inheriting from fully known
    // types. Since in JS we "inherit" from a variable which would
    // need to be resolved first, we for now dont handle the class
    // hierarchy.
    val astParentType     = methodAstParentStack.head.label
    val astParentFullName = methodAstParentStack.head.properties("FULL_NAME").toString

    val superClass = Try(createBabelNodeInfo(clazz.json("superClass")).code).toOption.toSeq
    val implements = Try(clazz.json("implements").arr.map(createBabelNodeInfo(_).code)).toOption.toSeq.flatten
    val mixins     = Try(clazz.json("mixins").arr.map(createBabelNodeInfo(_).code)).toOption.toSeq.flatten

    val typeDeclNode = createTypeDeclNode(
      typeName,
      typeFullName,
      parserResult.fullPath,
      s"class $typeName",
      astParentType,
      astParentFullName,
      inherits = superClass ++ implements ++ mixins
    )

    val classMetaTypeNode = createTypeNode(metaTypeName, metaTypeFullName)
    Ast.storeInDiffGraph(Ast(classMetaTypeNode), diffGraph)

    val metaTypeDeclNode = createTypeDeclNode(
      metaTypeName,
      metaTypeFullName,
      parserResult.fullPath,
      s"class $metaTypeName",
      astParentType,
      astParentFullName
    )

    diffGraph.addEdge(methodAstParentStack.head, typeDeclNode, EdgeTypes.AST)
    diffGraph.addEdge(methodAstParentStack.head, metaTypeDeclNode, EdgeTypes.AST)

    val metaTypeRefNode = createTypeRefNode(s"class $typeName", metaTypeFullName, clazz)

    methodAstParentStack.push(typeDeclNode)
    dynamicInstanceTypeStack.push(typeFullName)
    metaTypeRefIdStack.push(metaTypeRefNode)

    // In case there is no user-written constructor the JS parser creates
    // an empty one automatically. Hence, the following is safe:
    val constructorNode = classConstructor(typeName, clazz)

    val constructorBindingNode = createBindingNode()
    diffGraph.addEdge(metaTypeDeclNode, constructorBindingNode, EdgeTypes.BINDS)
    diffGraph.addEdge(constructorBindingNode, constructorNode, EdgeTypes.REF)

    val classBodyElements = classMembers(clazz, withConstructor = false)

    classBodyElements.foreach { classElement =>
      val memberId = createBabelNodeInfo(classElement) match {
        case m @ BabelNodeInfo(BabelAst.ClassMethod) =>
          val function                = createMethodNode(m)._2
          val fullName                = function.fullName.replace(s":${function.name}", s":$typeName:${function.name}")
          val classMethod             = function.fullName(fullName)
          val functionFullName        = classMethod.fullName
          val dynamicTypeHintFullName = Some(functionFullName)
          createMemberNode(classMethod.name, m.code, dynamicTypeHintFullName)
        case other =>
          // TODO: name field most likely needs adjustment
          createMemberNode(other.code, other.code, dynamicTypeOption = None)
      }

      if (classElement("static").bool) {
        diffGraph.addEdge(metaTypeDeclNode, memberId, EdgeTypes.AST)
      } else {
        diffGraph.addEdge(typeDeclNode, memberId, EdgeTypes.AST)
      }
    }

    methodAstParentStack.pop()
    dynamicInstanceTypeStack.pop()
    metaTypeRefIdStack.pop()

    List(Ast(metaTypeRefNode))
  }

  protected def astsForNode(json: Value): List[Ast] = createBabelNodeInfo(json) match {
    case BabelNodeInfo(BabelAst.File)                              => astsForNode(json("program"))
    case program @ BabelNodeInfo(BabelAst.Program)                 => astsForProgram(program)
    case exprStmt @ BabelNodeInfo(BabelAst.ExpressionStatement)    => astsForExpressionStatement(exprStmt)
    case callExpr @ BabelNodeInfo(BabelAst.CallExpression)         => astsForCallExpression(callExpr)
    case memberExpr @ BabelNodeInfo(BabelAst.MemberExpression)     => astsForMemberExpression(memberExpr)
    case ident @ BabelNodeInfo(BabelAst.Identifier)                => astsForIdentifier(ident)
    case stringLiteral @ BabelNodeInfo(BabelAst.StringLiteral)     => astsForStringLiteral(stringLiteral)
    case numLiteral @ BabelNodeInfo(BabelAst.NumericLiteral)       => astsForNumericLiteral(numLiteral)
    case func @ BabelNodeInfo(BabelAst.FunctionDeclaration)        => astsForFunctionDeclaration(func)
    case decl @ BabelNodeInfo(BabelAst.VariableDeclaration)        => astsForVariableDeclaration(decl)
    case assignment @ BabelNodeInfo(BabelAst.AssignmentExpression) => astsForAssignmentExpression(assignment)
    case binExpr @ BabelNodeInfo(BabelAst.BinaryExpression)        => astsForBinaryExpression(binExpr)
    case updateExpr @ BabelNodeInfo(BabelAst.UpdateExpression)     => astsForUpdateExpression(updateExpr)
    case unaryExpr @ BabelNodeInfo(BabelAst.UnaryExpression)       => astsForUnaryExpression(unaryExpr)
    case block @ BabelNodeInfo(BabelAst.BlockStatement)            => astsForBlockStatement(block)
    case ret @ BabelNodeInfo(BabelAst.ReturnStatement)             => astsForReturnStatement(ret)
    case seq @ BabelNodeInfo(BabelAst.SequenceExpression)          => astsForSequenceExpression(seq)
    case classExpr @ BabelNodeInfo(BabelAst.ClassExpression)       => astsForClass(classExpr)
    case classDecl @ BabelNodeInfo(BabelAst.ClassDeclaration)      => astsForClass(classDecl)
    case other                                                     => List(notHandledYet(other))
  }

  protected def astsForProgram(program: BabelNodeInfo): List[Ast] = createBlockStatementAsts(program.json("body"))

  protected def astsForNodes(jsons: List[Value]): List[Ast] = jsons.flatMap(astsForNode)

  protected def createProgramMethod(): Ast = {
    val absolutePath    = parserResult.fullPath
    val ast             = parserResult.json("ast")
    val lineNumber      = line(ast)
    val columnNumber    = column(ast)
    val lineNumberEnd   = lineEnd(ast)
    val columnNumberEnd = columnEnd(ast)
    val name            = ":program"
    val fullName        = parserResult.fullPath + ":" + name

    val programMethod =
      NewMethod()
        .name(name)
        .code(name)
        .fullName(fullName)
        .filename(absolutePath)
        .lineNumber(lineNumber)
        .lineNumberEnd(lineNumberEnd)
        .columnNumber(columnNumber)
        .columnNumberEnd(columnNumberEnd)
        .astParentType(NodeTypes.TYPE_DECL)
        .astParentFullName(fullName)

    methodAstParentStack.push(programMethod)

    val blockNode = NewBlock().typeFullName("ANY")

    scope.pushNewMethodScope(fullName, name, blockNode, None)
    localAstParentStack.push(blockNode)

    val thisParam = createParameterInNode("this", "this", 0, isVariadic = false, lineNumber, columnNumber)

    val methodChildren = astsForNode(ast)

    val methodReturn = NewMethodReturn()
      .code("RET")
      .evaluationStrategy(EvaluationStrategies.BY_VALUE)
      .typeFullName(Defines.ANY.label)

    localAstParentStack.pop()
    scope.popScope()
    methodAstParentStack.pop()

    val functionTypeAndTypeDeclAst =
      createFunctionTypeAndTypeDecl(programMethod, methodAstParentStack.head, name, fullName, absolutePath)
    Ast.storeInDiffGraph(functionTypeAndTypeDeclAst, diffGraph)

    methodAst(programMethod, List(thisParam), Ast(blockNode).withChildren(methodChildren), methodReturn)
  }

  protected def createVariableReferenceLinks(): Unit = {
    val resolvedReferenceIt = scope.resolve(createMethodLocalForUnresolvedReference)
    val capturedLocals      = mutable.HashMap.empty[String, NewNode]

    resolvedReferenceIt.foreach { case ResolvedReference(variableNodeId, origin) =>
      var currentScope             = origin.stack
      var currentReferenceId       = origin.referenceNodeId
      var nextReferenceId: NewNode = null

      var done = false
      while (!done) {
        val localOrCapturedLocalIdOption =
          if (currentScope.get.nameToVariableNode.contains(origin.variableName)) {
            done = true
            Some(variableNodeId)
          } else {
            currentScope.flatMap {
              case methodScope: MethodScopeElement =>
                // We have reached a MethodScope and still did not find a local variable to link to.
                // For all non local references the CPG format does not allow us to link
                // directly. Instead we need to create a fake local variable in method
                // scope and link to this local which itself carries the information
                // that it is a captured variable. This needs to be done for each
                // method scope until we reach the originating scope.
                val closureBindingIdProperty =
                  methodScope.methodFullName + ":" + origin.variableName
                capturedLocals
                  .updateWith(closureBindingIdProperty) {
                    case None =>
                      val methodScopeNodeId = methodScope.scopeNode
                      val localId =
                        createLocalNode(origin.variableName, Defines.ANY.label, Some(closureBindingIdProperty))
                      diffGraph.addEdge(methodScopeNodeId, localId, EdgeTypes.AST)
                      val closureBindingId = createClosureBindingNode(closureBindingIdProperty, origin.variableName)
                      methodScope.capturingRefId.foreach(ref =>
                        diffGraph.addEdge(ref, closureBindingId, EdgeTypes.CAPTURE)
                      )
                      nextReferenceId = closureBindingId
                      Some(localId)
                    case someLocalId =>
                      // When there is already a LOCAL representing the capturing, we do not
                      // need to process the surrounding scope element as this has already
                      // been processed.
                      done = true
                      someLocalId
                  }
              case _: BlockScopeElement => None
            }
          }

        localOrCapturedLocalIdOption.foreach { localOrCapturedLocalId =>
          diffGraph.addEdge(currentReferenceId, localOrCapturedLocalId, EdgeTypes.REF)
          currentReferenceId = nextReferenceId
        }

        currentScope = currentScope.get.surroundingScope
      }
    }
  }

  protected def createMethodLocalForUnresolvedReference(
    methodScopeNodeId: NewNode,
    variableName: String
  ): (NewNode, ScopeType) = {
    val local = createLocalNode(variableName, Defines.ANY.label)
    diffGraph.addEdge(methodScopeNodeId, local, EdgeTypes.AST)
    (local, MethodScope)
  }

}
