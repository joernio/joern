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
  private val methodAstParentStack: Stack[NewNode]            = new Stack[NewNode]()
  private val localAstParentStack: Stack[NewBlock]            = new Stack[NewBlock]()
  private val usedVariableNames: mutable.HashMap[String, Int] = mutable.HashMap.empty[String, Int]
  private val functionNodeToNameAndFullName                   = mutable.HashMap.empty[BabelNodeInfo, (String, String)]
  private val functionFullNames                               = mutable.HashSet.empty[String]
  private val metaTypeRefIdStack                              = mutable.Stack.empty[NewTypeRef]

  protected val dynamicInstanceTypeStack: mutable.Seq[String] = mutable.Stack.empty[String]

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
        // case _ if func.isAnonymous && func.isClassConstructor =>
        //  "anonClass<constructor>"
        // case _ if func.isAnonymous => TODO: is this a ArrowFunctionExpression?
        //  "anonymous"
        // case _ if func.isClassConstructor =>
        //  s"${func.getName}<constructor>"
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

  protected def astsForFunctionDeclaration(
    func: BabelNodeInfo,
    order: Int,
    shouldCreateFunctionReference: Boolean = false,
    shouldCreateAssignmentCall: Boolean = false
  ): Seq[Ast] = {
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

    val bodyStmtAsts = createBlockStatementAsts(func.json("body")("body"))

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

    callAsts ++ methodRefNode.map(Ast(_)).toSeq
  }

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
      case None => Seq(Ast(localNode))
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
    case block @ BabelNodeInfo(BabelAst.BlockStatement)            => astsForBlockStatement(block, order)
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
