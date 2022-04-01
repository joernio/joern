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
import io.joern.x2cpg.passes.frontend.MetaDataPass
import io.shiftleft.codepropertygraph.generated.DispatchTypes
import io.shiftleft.codepropertygraph.generated.EdgeTypes
import io.shiftleft.codepropertygraph.generated.ModifierTypes
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.semanticcpg.language.types.structure.NamespaceTraversal
import org.slf4j.{Logger, LoggerFactory}
import ujson.Obj
import ujson.Value

import scala.collection.mutable
import scala.util.Try

class AstCreator(val config: Config, val parserResult: ParseResult, val global: Global)
    extends AstCreatorBase(parserResult.filename)
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
    val name    = parserResult.filename
    val cpgFile = Ast(NewFile().name(name).order(0))
    val ast     = cpgFile.withChild(astForFileGlobal())
    Ast.storeInDiffGraph(ast, diffGraph)
    createVariableReferenceLinks()
    diffGraph
  }

  protected def astsForExpressionStatement(exprStmt: BabelNodeInfo, order: Int): Seq[Ast] =
    astsForNode(exprStmt.json("expression"), order)

  protected def createBuiltinStaticCall(
    callExpr: BabelNodeInfo,
    callee: BabelNodeInfo,
    methodFullName: String,
    order: Int
  ): Ast = {
    val methodName = callee.node match {
      case BabelAst.MemberExpression =>
        code(callee.json("property"))
      case BabelAst.Identifier =>
        callee.code
      case _ => callee.code
    }
    val callNode =
      createStaticCallNode(callExpr.code, methodName, methodFullName, order, callee.lineNumber, callee.columnNumber)
    val args = astsForNodes(callExpr.json("arguments").arr.toSeq)
    Ast(callNode).withChildren(args).withArgEdges(callNode, args)
  }

  protected def handleCallNodeArgs(
    callExpr: BabelNodeInfo,
    receiverNode: NewNode,
    baseNode: NewNode,
    functionBaseNode: NewNode,
    functionPropertyNode: Option[NewFieldIdentifier]
  ): Seq[Ast] = {
    val args = astsForNodes(callExpr.json("arguments").arr.toSeq)

    val baseCode = codeOf(functionBaseNode)
    val propertyCode = functionPropertyNode match {
      case Some(id) => "." + codeOf(id)
      case None     => ""
    }

    val argsCode = args.map(a => codeOf(a.nodes.head)).mkString("(", ", ", ")")
    val code     = s"$baseCode$propertyCode$argsCode"

    val callNode = createCallNode(code, "", DispatchTypes.DYNAMIC_DISPATCH, callExpr.lineNumber, callExpr.columnNumber)

    addOrder(receiverNode, 0)
    addOrder(baseNode, 1)
    addArgumentIndex(baseNode, 0)

    var currOrder    = 2
    var currArgIndex = 1
    args.foreach { ast =>
      addOrder(ast.nodes.head, currOrder)
      addArgumentIndex(ast.nodes.head, currArgIndex)
      currOrder += 1
      currArgIndex += 1
    }

    Seq(
      Ast(callNode)
        .withChild(Ast(receiverNode))
        .withReceiverEdge(callNode, receiverNode)
        .withChild(Ast(baseNode))
        .withArgEdge(callNode, baseNode)
        .withChildren(args)
        .withArgEdges(callNode, args)
    )
  }

  protected def astsForCallExpression(callExpr: BabelNodeInfo, order: Int): Seq[Ast] = {
    val callee         = createBabelNodeInfo(callExpr.json("callee"))
    val methodFullName = callee.code
    val callNode = if (GlobalBuiltins.builtins.contains(methodFullName)) {
      Seq(createBuiltinStaticCall(callExpr, callee, methodFullName, order))
    } else {
      val (functionBaseAsts, functionPropertyNode, receiverAsts, baseNode) = callee.node match {
        case BabelAst.MemberExpression =>
          // "this" argument is coming from source.
          val base = createBabelNodeInfo(callee.json("object"))
          base.node match {
            case BabelAst.Identifier =>
              val receiverAsts = astsForNode(callee.json, 0)
              receiverAsts.foreach(Ast.storeInDiffGraph(_, diffGraph))
              val baseNode = createIdentifierNode(base.code, base).order(1).argumentIndex(1)
              scope.addVariableReference(base.code, baseNode)
              (receiverAsts, None, receiverAsts, baseNode)
            case _ =>
              // TODO: check for used nodes
              val tmpVarName  = generateUnusedVariableName(usedVariableNames, Set.empty, "_tmp")
              val baseTmpNode = createIdentifierNode(tmpVarName, base)
              scope.addVariableReference(tmpVarName, baseTmpNode)
              val baseAsts = astsForNode(base.json, 2)
              baseAsts.foreach(Ast.storeInDiffGraph(_, diffGraph))
              val code = s"(${codeOf(baseTmpNode)} = ${base.code})"
              val tmpAssignmentAst =
                createAssignment(baseTmpNode, baseAsts.head.nodes.head, code, 1, base.lineNumber, base.columnNumber)
              val member     = createBabelNodeInfo(callee.json("property"))
              val memberNode = createFieldIdentifierNode(member.code, member.lineNumber, member.columnNumber)
              val fieldAccessAst =
                createFieldAccess(tmpAssignmentAst.nodes.head, memberNode, 0, callee.lineNumber, callee.columnNumber)
              val thisTmpNode = createIdentifierNode(tmpVarName, callee)
              scope.addVariableReference(tmpVarName, thisTmpNode)

              Ast.storeInDiffGraph(tmpAssignmentAst, diffGraph)
              Ast.storeInDiffGraph(fieldAccessAst, diffGraph)

              (baseAsts, Some(memberNode), Seq(fieldAccessAst), thisTmpNode)
          }
        case _ =>
          val receiverAsts = astsForNode(callee.json, order)
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
    callNode
  }

  protected def astsForMemberExpression(memberExpr: BabelNodeInfo, order: Int): Seq[Ast] = {
    val baseAsts = astsForNode(memberExpr.json("object"), 1)
    baseAsts.foreach(Ast.storeInDiffGraph(_, diffGraph))
    val memberNode =
      createFieldIdentifierNode(code(memberExpr.json("property")), memberExpr.lineNumber, memberExpr.columnNumber)
    val accessAst =
      createFieldAccess(baseAsts.head.nodes.head, memberNode, order, memberExpr.lineNumber, memberExpr.columnNumber)
    Seq(accessAst)
  }

  protected def astsForIdentifier(ident: BabelNodeInfo, order: Int): Seq[Ast] = {
    val name      = ident.json("name").str
    val identNode = createIdentifierNode(name, ident).order(order).argumentIndex(order)
    scope.addVariableReference(name, identNode)
    Seq(Ast(identNode))
  }

  protected def astsForStringLiteral(stringLiteral: BabelNodeInfo, order: Int): Seq[Ast] =
    Seq(
      Ast(
        createLiteralNode(
          stringLiteral.code,
          Some(Defines.STRING.label),
          order,
          stringLiteral.lineNumber,
          stringLiteral.columnNumber
        )
      )
    )

  protected def astsForNumericLiteral(numericLiteral: BabelNodeInfo, order: Int): Seq[Ast] =
    Seq(
      Ast(
        createLiteralNode(
          numericLiteral.code,
          Some(Defines.NUMBER.label),
          order,
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
        case _ if func.json("key").isNull && func.json("kind").str == "constructor" =>
          "anonClass<constructor>"
        // case _ if func.isAnonymous => TODO: is this a ArrowFunctionExpression?
        //  "anonymous"
        case _ if func.json("kind").str == "method" =>
          func.json("key")("name").str
        case _ if func.json("kind").str == "constructor" =>
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
        val fullNamePrefix = parserResult.filename + ":" + computeScopePath(scope.getScopeHead) + ":"
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
    order: Int,
    shouldCreateFunctionReference: Boolean = false,
    shouldCreateAssignmentCall: Boolean = false
  ): (Seq[Ast], NewMethod) = {
    val (methodName, methodFullName) = calcMethodNameAndFullName(func)
    val methodRefNode = if (!shouldCreateFunctionReference) {
      None
    } else { Some(createMethodRefNode(methodName, methodFullName, func)) }

    val callAsts = if (shouldCreateAssignmentCall && shouldCreateFunctionReference) {
      val idNode  = createIdentifierNode(methodName, func)
      val idLocal = createLocalNode(methodName, methodFullName, 0)
      diffGraph.addEdge(localAstParentStack.head, idLocal, EdgeTypes.AST)
      scope.addVariable(methodName, idLocal, BlockScope)
      val code       = s"$methodName = ${func.code}"
      val assignment = createAssignment(idNode, methodRefNode.get, code, order, func.lineNumber, func.columnNumber)
      Seq(assignment)
    } else {
      Seq.empty
    }

    val methodNode          = createMethodNode(methodName, methodFullName, func).order(order)
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

    val thisNode = createParameterInNode("this", "this", 0, line = func.lineNumber, column = func.columnNumber)

    val paramNodes = withOrder(func.json("params").arr.toSeq) { (p, o) =>
      createBabelNodeInfo(p) match {
        case rest @ BabelNodeInfo(BabelAst.RestElement) =>
          createParameterInNode(rest.code.replace("...", ""), rest.code, o, rest.lineNumber, rest.columnNumber)
            .isVariadic(true)
        case other =>
          createParameterInNode(other.code, other.code, o, other.lineNumber, other.columnNumber)
      }
    }

    localAstParentStack.push(blockNode)

    val bodyStmtAsts = createBlockStatementAsts(block("body"))

    val methodReturnNode = createMethodReturnNode(func).order(2)

    localAstParentStack.pop()
    scope.popScope()
    methodAstParentStack.pop()

    val functionTypeAndTypeDeclAst =
      createFunctionTypeAndTypeDecl(
        methodNode,
        methodAstParentStack.head,
        methodName,
        methodFullName,
        parserResult.filename
      )

    val mAst =
      methodAst(methodNode, thisNode +: paramNodes, Ast(blockNode).withChildren(bodyStmtAsts), methodReturnNode)
        .withChild(Ast(virtualModifierNode))

    Ast.storeInDiffGraph(mAst, diffGraph)
    Ast.storeInDiffGraph(functionTypeAndTypeDeclAst, diffGraph)
    diffGraph.addEdge(methodAstParentStack.head, methodNode, EdgeTypes.AST)

    (callAsts ++ methodRefNode.map(Ast(_)).toSeq, methodNode)
  }

  protected def astsForFunctionDeclaration(
    func: BabelNodeInfo,
    order: Int,
    shouldCreateFunctionReference: Boolean = false,
    shouldCreateAssignmentCall: Boolean = false
  ): Seq[Ast] = createMethodNode(func, order, shouldCreateFunctionReference, shouldCreateAssignmentCall)._1

  protected def astsForVariableDeclarator(declarator: Value, order: Int, scopeType: ScopeType): Seq[Ast] = {
    val id   = createBabelNodeInfo(declarator("id"))
    val init = Try(createBabelNodeInfo(declarator("init"))).toOption

    val typeFullName = init match {
      case Some(f @ BabelNodeInfo(BabelAst.FunctionDeclaration)) =>
        val (_, methodFullName) = calcMethodNameAndFullName(f)
        methodFullName
      case _ => Defines.ANY.label
    }

    val localNode = createLocalNode(id.code, typeFullName, 0)
    diffGraph.addEdge(localAstParentStack.head, localNode, EdgeTypes.AST)
    scope.addVariable(id.code, localNode, scopeType)

    init match {
      case Some(f @ BabelNodeInfo(BabelAst.FunctionDeclaration)) =>
        val destAsts   = astsForNode(id.json, 1)
        val sourceAsts = astsForFunctionDeclaration(f, 2, shouldCreateFunctionReference = true)
        val assigmentCallAst =
          createAssignment(
            destAsts.head.nodes.head,
            sourceAsts.head.nodes.head,
            code(declarator),
            order,
            line = line(declarator),
            column = column(declarator)
          )
        Seq(assigmentCallAst) ++ destAsts ++ sourceAsts
      case Some(initExpr) =>
        val destAsts   = astsForNode(id.json, 1)
        val sourceAsts = astsForNode(initExpr.json, 2)
        val assigmentCallAst =
          createAssignment(
            destAsts.head.nodes.head,
            sourceAsts.head.nodes.head,
            code(declarator),
            order,
            line = line(declarator),
            column = column(declarator)
          )
        Seq(assigmentCallAst) ++ destAsts ++ sourceAsts
      case None => Seq.empty
    }
  }

  protected def astsForVariableDeclaration(declaration: BabelNodeInfo, order: Int): Seq[Ast] = {
    val scopeType = if (declaration.json("kind").str == "let") {
      BlockScope
    } else {
      MethodScope
    }

    withOrder(declaration.json("declarations").arr.toSeq) { (d, o) =>
      astsForVariableDeclarator(d, order + o - 1, scopeType)
    }.flatten
  }

  protected def astsForAssignmentExpression(assignment: BabelNodeInfo, order: Int): Seq[Ast] = {
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

    val lhsAsts = astsForNode(assignment.json("left"), 1)
    val rhsAsts = astsForNode(assignment.json("right"), 2)

    val callNode =
      createCallNode(assignment.code, op, DispatchTypes.STATIC_DISPATCH, assignment.lineNumber, assignment.columnNumber)
        .order(order)

    Seq(
      Ast(callNode)
        .withChildren(lhsAsts)
        .withChildren(rhsAsts)
        .withArgEdges(callNode, lhsAsts)
        .withArgEdges(callNode, rhsAsts)
    )
  }

  protected def createBlockStatementAsts(json: Value): Seq[Ast] = {
    val blockStmts = json.arr.map(createBabelNodeInfo).sortBy(_.node != BabelAst.FunctionDeclaration).toSeq
    withOrder(blockStmts) { (n, o) =>
      n match {
        case func @ BabelNodeInfo(BabelAst.FunctionDeclaration) =>
          astsForFunctionDeclaration(func, o, shouldCreateAssignmentCall = true, shouldCreateFunctionReference = true)
        case _ => astsForNode(n.json, o)
      }
    }.flatten
  }

  protected def astsForBlockStatement(block: BabelNodeInfo, order: Int): Seq[Ast] = {
    val blockNode = createBlockNode(block.code, order, block.lineNumber, block.columnNumber)
    scope.pushNewBlockScope(blockNode)
    localAstParentStack.push(blockNode)
    val blockStatementAsts = createBlockStatementAsts(block.json("body"))
    localAstParentStack.pop()
    scope.popScope()
    Seq(Ast(blockNode).withChildren(blockStatementAsts))
  }

  protected def astsForBinaryExpression(binExpr: BabelNodeInfo, order: Int): Seq[Ast] = {
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

    val lhsAsts = astsForNode(binExpr.json("left"), 1)
    val rhsAsts = astsForNode(binExpr.json("right"), 2)

    val callNode =
      createCallNode(binExpr.code, op, DispatchTypes.STATIC_DISPATCH, binExpr.lineNumber, binExpr.columnNumber)
        .order(order)

    Seq(
      Ast(callNode)
        .withChildren(lhsAsts)
        .withChildren(rhsAsts)
        .withArgEdges(callNode, lhsAsts)
        .withArgEdges(callNode, rhsAsts)
    )
  }

  protected def astsForUpdateExpression(updateExpr: BabelNodeInfo, order: Int): Seq[Ast] = {
    val op = updateExpr.json("operator").str match {
      case "++" => Operators.preIncrement
      case "--" => Operators.preDecrement
      case other =>
        logger.warn(s"Unknown update operator: '$other'")
        Operators.assignment
    }

    val argumentAsts = astsForNode(updateExpr.json("argument"), 1)

    val callNode =
      createCallNode(updateExpr.code, op, DispatchTypes.STATIC_DISPATCH, updateExpr.lineNumber, updateExpr.columnNumber)
        .order(order)

    Seq(
      Ast(callNode)
        .withChildren(argumentAsts)
        .withArgEdges(callNode, argumentAsts)
    )
  }

  protected def astsForUnaryExpression(unaryExpr: BabelNodeInfo, order: Int): Seq[Ast] = {
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

    val argumentAsts = astsForNode(unaryExpr.json("argument"), 1)

    val callNode =
      createCallNode(unaryExpr.code, op, DispatchTypes.STATIC_DISPATCH, unaryExpr.lineNumber, unaryExpr.columnNumber)
        .order(order)

    Seq(
      Ast(callNode)
        .withChildren(argumentAsts)
        .withArgEdges(callNode, argumentAsts)
    )
  }

  protected def astsForReturnStatement(ret: BabelNodeInfo, order: Int): Seq[Ast] = {
    val retNode = createReturnNode(ret).order(order).argumentIndex(order)
    safeObj(ret.json, "argument").map { argument =>
      val argAsts = astsForNode(Obj(argument), 1)
      Ast(retNode).withChildren(argAsts).withArgEdges(retNode, argAsts)
    }.toSeq
  }

  protected def astsForSequenceExpression(seq: BabelNodeInfo, order: Int): Seq[Ast] = {
    val blockNode = createBlockNode(seq.code, order, seq.lineNumber, seq.columnNumber)
    scope.pushNewBlockScope(blockNode)
    localAstParentStack.push(blockNode)
    val sequenceExpressionAsts = createBlockStatementAsts(seq.json("expressions"))
    localAstParentStack.pop()
    scope.popScope()
    Seq(Ast(blockNode).withChildren(sequenceExpressionAsts))
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
        val fullNamePrefix   = parserResult.filename + ":" + computeScopePath(scope.getScopeHead) + ":"
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

  private def classConstructor(typeName: String, classExpr: BabelNodeInfo): NewMethod = {
    val maybeClassConstructor = Try(classExpr.json("body")("body").arr.find(_("kind").str == "constructor")).toOption
    val methodNode = maybeClassConstructor match {
      case Some(Some(classConstructor)) => createMethodNode(createBabelNodeInfo(classConstructor), 0)._2
      case _ =>
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
        createMethodNode(createBabelNodeInfo(ujson.read(fakeConstructorCode)), 0)._2
    }
    if (!typeName.startsWith("_anon_cdecl")) {
      val name     = methodNode.name.replace("<", s"$typeName<")
      val fullName = methodNode.fullName.replace("<", s"$typeName<")
      methodNode.name(name).fullName(fullName)
    } else {
      methodNode
    }
  }

  protected def astsForClass(clazz: BabelNodeInfo, order: Int): Seq[Ast] = {
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

    val typeDeclId = createTypeDeclNode(
      typeName,
      typeFullName,
      parserResult.filename,
      s"class $typeName",
      astParentType,
      astParentFullName,
      order = order,
      inherits = superClass ++ implements ++ mixins
    )

    val classMetaTypeNode = createTypeNode(metaTypeName, metaTypeFullName)
    Ast.storeInDiffGraph(Ast(classMetaTypeNode), diffGraph)

    val metaTypeDeclId = createTypeDeclNode(
      metaTypeName,
      metaTypeFullName,
      parserResult.filename,
      s"class $metaTypeName",
      astParentType,
      astParentFullName,
      order = order
    )

    diffGraph.addEdge(methodAstParentStack.head, typeDeclId, EdgeTypes.AST)
    diffGraph.addEdge(methodAstParentStack.head, metaTypeDeclId, EdgeTypes.AST)

    val metaTypeRefId = createTypeRefNode(s"class $typeName", metaTypeFullName, clazz)

    methodAstParentStack.push(typeDeclId)
    dynamicInstanceTypeStack.push(typeFullName)
    metaTypeRefIdStack.push(metaTypeRefId)

    // In case there is no user-written constructor the JS parser creates
    // an empty one automatically. Hence, the following is safe:
    val constructorId = classConstructor(typeName, clazz)

    val constructorBindingId = createBindingNode()
    diffGraph.addEdge(metaTypeDeclId, constructorBindingId, EdgeTypes.BINDS)
    diffGraph.addEdge(constructorBindingId, constructorId, EdgeTypes.REF)

    val classBodyElements = clazz.json("body")("body").arr.filterNot(_("kind").str == "constructor").toSeq

    withOrder(classBodyElements) { (classElement, o) =>
      val memberId = createBabelNodeInfo(classElement) match {
        case m @ BabelNodeInfo(BabelAst.ClassMethod) =>
          val function = createMethodNode(m, o)._2
          val classMethod = if (!typeName.startsWith("_anon_cdecl")) {
            val fullName = function.fullName.replace(s":${function.name}", s":$typeName:${function.name}")
            function.fullName(fullName)
          } else {
            function
          }
          val functionFullName        = classMethod.fullName
          val dynamicTypeHintFullName = Some(functionFullName)
          createMemberNode(classMethod.name, m.code, dynamicTypeHintFullName)
        case other =>
          // TODO: unclear
          createMemberNode(other.code, other.code, dynamicTypeOption = None)
      }

      if (classElement("static").bool) {
        diffGraph.addEdge(metaTypeDeclId, memberId, EdgeTypes.AST)
      } else {
        diffGraph.addEdge(typeDeclId, memberId, EdgeTypes.AST)
      }
    }

    methodAstParentStack.pop()
    dynamicInstanceTypeStack.pop()
    metaTypeRefIdStack.pop()

    Seq(Ast(metaTypeRefId))
  }

  protected def astsForNode(json: Value, order: Int): Seq[Ast] = createBabelNodeInfo(json) match {
    case BabelNodeInfo(BabelAst.File)                              => astsForNode(json("program"), order)
    case program @ BabelNodeInfo(BabelAst.Program)                 => astsForProgram(program)
    case exprStmt @ BabelNodeInfo(BabelAst.ExpressionStatement)    => astsForExpressionStatement(exprStmt, order)
    case callExpr @ BabelNodeInfo(BabelAst.CallExpression)         => astsForCallExpression(callExpr, order)
    case memberExpr @ BabelNodeInfo(BabelAst.MemberExpression)     => astsForMemberExpression(memberExpr, order)
    case ident @ BabelNodeInfo(BabelAst.Identifier)                => astsForIdentifier(ident, order)
    case stringLiteral @ BabelNodeInfo(BabelAst.StringLiteral)     => astsForStringLiteral(stringLiteral, order)
    case numLiteral @ BabelNodeInfo(BabelAst.NumericLiteral)       => astsForNumericLiteral(numLiteral, order)
    case func @ BabelNodeInfo(BabelAst.FunctionDeclaration)        => astsForFunctionDeclaration(func, order)
    case decl @ BabelNodeInfo(BabelAst.VariableDeclaration)        => astsForVariableDeclaration(decl, order)
    case assignment @ BabelNodeInfo(BabelAst.AssignmentExpression) => astsForAssignmentExpression(assignment, order)
    case binExpr @ BabelNodeInfo(BabelAst.BinaryExpression)        => astsForBinaryExpression(binExpr, order)
    case updateExpr @ BabelNodeInfo(BabelAst.UpdateExpression)     => astsForUpdateExpression(updateExpr, order)
    case unaryExpr @ BabelNodeInfo(BabelAst.UnaryExpression)       => astsForUnaryExpression(unaryExpr, order)
    case block @ BabelNodeInfo(BabelAst.BlockStatement)            => astsForBlockStatement(block, order)
    case ret @ BabelNodeInfo(BabelAst.ReturnStatement)             => astsForReturnStatement(ret, order)
    case seq @ BabelNodeInfo(BabelAst.SequenceExpression)          => astsForSequenceExpression(seq, order)
    case classExpr @ BabelNodeInfo(BabelAst.ClassExpression)       => astsForClass(classExpr, order)
    case classDecl @ BabelNodeInfo(BabelAst.ClassDeclaration)      => astsForClass(classDecl, order)
    case other                                                     => Seq(notHandledYet(other, order))
  }

  protected def astsForProgram(program: BabelNodeInfo): Seq[Ast] =
    createBlockStatementAsts(program.json("body"))

  protected def astsForNodes(jsons: Seq[Value]): Seq[Ast] = withOrder(jsons) { (n, o) =>
    astsForNode(n, o)
  }.flatten

  protected def createProgramMethod(): Ast = {
    val absolutePath    = parserResult.filename
    val ast             = parserResult.json("ast")
    val lineNumber      = line(ast)
    val columnNumber    = column(ast)
    val lineNumberEnd   = lineEnd(ast)
    val columnNumberEnd = columnEnd(ast)
    val name            = ":program"
    val fullName        = parserResult.filename + ":" + name

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

    val blockNode = NewBlock()
      .order(1)
      .argumentIndex(1)
      .typeFullName("ANY")

    scope.pushNewMethodScope(fullName, name, blockNode, None)
    localAstParentStack.push(blockNode)

    val thisParam = createParameterInNode("this", "this", 0, lineNumber, columnNumber)

    val methodChildren = astsForNode(ast, 1)

    val methodReturn = NewMethodReturn()
      .code("RET")
      .evaluationStrategy(EvaluationStrategies.BY_VALUE)
      .typeFullName(Defines.ANY.label)
      .order(2)

    localAstParentStack.pop()
    scope.popScope()
    methodAstParentStack.pop()

    val functionTypeAndTypeDeclAst =
      createFunctionTypeAndTypeDecl(programMethod, methodAstParentStack.head, name, fullName, absolutePath)
    Ast.storeInDiffGraph(functionTypeAndTypeDeclAst, diffGraph)

    methodAst(programMethod, Seq(thisParam), Ast(blockNode).withChildren(methodChildren), methodReturn)
  }

  private def astForFileGlobal(): Ast = {
    val absolutePath = parserResult.filename
    val name         = NamespaceTraversal.globalNamespaceName
    val fullName     = MetaDataPass.getGlobalNamespaceBlockFullName(Some(absolutePath))
    val namespaceBlock = NewNamespaceBlock()
      .name(name)
      .fullName(fullName)
      .filename(absolutePath)
      .order(1)
    methodAstParentStack.push(namespaceBlock)
    Ast(namespaceBlock).withChild(createProgramMethod())
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
                        createLocalNode(origin.variableName, Defines.ANY.label, 0, Some(closureBindingIdProperty))
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
    val local = createLocalNode(variableName, Defines.ANY.label, 0)
    diffGraph.addEdge(methodScopeNodeId, local, EdgeTypes.AST)
    (local, MethodScope)
  }

}
