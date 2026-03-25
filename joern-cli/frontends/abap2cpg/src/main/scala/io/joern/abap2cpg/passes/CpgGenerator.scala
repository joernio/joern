package io.joern.abap2cpg.passes

import io.joern.abap2cpg.parser.AbapIntermediateAst
import io.joern.abap2cpg.parser.AbapIntermediateAst.*
import io.shiftleft.codepropertygraph.generated.*
import io.shiftleft.codepropertygraph.generated.nodes.*

/** Generates CPG nodes from ABAP intermediate AST
  *
  * Focuses on basic nodes: metadata, file, method, parameters
  */
class CpgGenerator {

  /** Represents a CPG node with its children (for nested structures like CALL with ARGUMENTs) */
  private case class NodeWithChildren(
    node: NewNode,
    children: Seq[NodeWithChildren] = Seq.empty
  )

  /** Generate CPG from ABAP program */
  def generateCpg(program: ProgramRoot): DiffGraphBuilder = {
    val diffGraph = Cpg.newDiffGraphBuilder

    // Track all METHOD and CALL nodes for later CALL edge creation
    var allMethods = scala.collection.mutable.ArrayBuffer[NewMethod]()
    var allCalls = scala.collection.mutable.ArrayBuffer[(NewCall, Option[String])]() // (call, className)

    // Create metadata node
    val metaData = NewMetaData()
      .language("NEWC")
      .version("1.0")
      .root(program.fileName)

    diffGraph.addNode(metaData)

    // Create file node
    val fileNode = NewFile()
      .name(program.fileName)
      .order(0)

    diffGraph.addNode(fileNode)

    // Create namespace block (global scope for file)
    val namespaceBlock = NewNamespaceBlock()
      .name(program.fileName)
      .fullName(program.fileName)
      .filename(program.fileName)
      .order(1)

    diffGraph.addNode(namespaceBlock)
    diffGraph.addEdge(fileNode, namespaceBlock, EdgeTypes.AST)

    var methodOrder = 1

    // Create classes with TYPE_DECL nodes
    if (program.classes.nonEmpty) {
      program.classes.foreach { classDef =>
        // Create TYPE_DECL node for the class
        val typeDecl = NewTypeDecl()
          .name(classDef.name)
          .fullName(classDef.name)
          .isExternal(false)
          .filename(program.fileName)

        classDef.span.start.foreach { pos =>
          typeDecl.lineNumber(pos.row)
          typeDecl.columnNumber(pos.col)
        }

        diffGraph.addNode(typeDecl)
        diffGraph.addEdge(namespaceBlock, typeDecl, EdgeTypes.AST)

        // Create methods under TYPE_DECL
        classDef.methods.foreach { method =>
          val (methodNode, methodChildren, (blockNode, blockChildren)) = createMethodWithChildren(method, Some(classDef.name), program.fileName, methodOrder)
          diffGraph.addNode(methodNode)

          // Track method for CALL edge creation
          allMethods += methodNode

          // Add AST edge from TYPE_DECL to method (not namespace)
          diffGraph.addEdge(typeDecl, methodNode, EdgeTypes.AST)

          // Add method children (parameters, return, block)
          methodChildren.foreach { childNode =>
            diffGraph.addNode(childNode)
            diffGraph.addEdge(methodNode, childNode, EdgeTypes.AST)
          }

          // Add block node
          diffGraph.addNode(blockNode)
          diffGraph.addEdge(methodNode, blockNode, EdgeTypes.AST)

          // Add block children (call nodes, etc.) with nested structures
          blockChildren.foreach { stmt =>
            addNodeWithChildren(diffGraph, blockNode, stmt)
            // Collect CALL nodes for later CALL edge creation
            collectCallNodes(stmt, Some(classDef.name), allCalls)
          }

          // Add CFG edges between statements
          addCfgEdges(diffGraph, methodNode, methodChildren, blockChildren)

          methodOrder += 1
        }
      }
    } else {
      // Create standalone methods (for programs/forms without classes)
      program.methods.foreach { method =>
        val (methodNode, methodChildren, (blockNode, blockChildren)) = createMethodWithChildren(method, None, program.fileName, methodOrder)
        diffGraph.addNode(methodNode)

        // Track method for CALL edge creation
        allMethods += methodNode

        // Add AST edge from namespace to method
        diffGraph.addEdge(namespaceBlock, methodNode, EdgeTypes.AST)

        // Add method children (parameters, return, block)
        methodChildren.foreach { childNode =>
          diffGraph.addNode(childNode)
          diffGraph.addEdge(methodNode, childNode, EdgeTypes.AST)
        }

        // Add block node
        diffGraph.addNode(blockNode)
        diffGraph.addEdge(methodNode, blockNode, EdgeTypes.AST)

        // Add block children (call nodes, etc.) with nested structures
        blockChildren.foreach { stmt =>
          addNodeWithChildren(diffGraph, blockNode, stmt)
          // Collect CALL nodes for later CALL edge creation
          collectCallNodes(stmt, None, allCalls)
        }

        // Add CFG edges between statements
        addCfgEdges(diffGraph, methodNode, methodChildren, blockChildren)

        methodOrder += 1
      }
    }

    // Add CALL edges from CALL nodes to METHOD nodes
    addCallEdges(diffGraph, allMethods.toSeq, allCalls.toSeq)

    diffGraph
  }

  /** Recursively add a node with its children to the diff graph */
  private def addNodeWithChildren(
    diffGraph: DiffGraphBuilder,
    parentNode: NewNode,
    nodeWithChildren: NodeWithChildren
  ): Unit = {
    // Add the node itself
    diffGraph.addNode(nodeWithChildren.node)
    // Add AST edge from parent
    diffGraph.addEdge(parentNode, nodeWithChildren.node, EdgeTypes.AST)

    // If this node has argumentIndex set (non-zero), also add ARGUMENT edge
    nodeWithChildren.node match {
      case expr: ExpressionNew if expr.argumentIndex != 0 =>
        diffGraph.addEdge(parentNode, expr, EdgeTypes.ARGUMENT)
      case _ => ()
    }

    // Recursively add children
    nodeWithChildren.children.foreach { child =>
      addNodeWithChildren(diffGraph, nodeWithChildren.node, child)
    }
  }

  /** Create METHOD node with parameters, returning (methodNode, methodChildren, blockAndChildren) */
  private def createMethodWithChildren(
    method: MethodDef,
    className: Option[String],
    fileName: String,
    order: Int
  ): (NewMethod, Seq[NewNode], (NewBlock, Seq[NodeWithChildren])) = {
    val fullName = className match {
      case Some(cls) => s"$cls::${method.name}"
      case None => method.name
    }

    val signature = createSignature(method.parameters)

    val methodNode = NewMethod()
      .name(method.name)
      .fullName(fullName)
      .signature(signature)
      .filename(fileName)
      .isExternal(false)
      .order(order)

    // Add line/column numbers if available
    method.span.start.foreach { pos =>
      methodNode.lineNumber(pos.row)
      methodNode.columnNumber(pos.col)
    }

    var childNodes = Seq[NewNode]()

    // Create IMPORTING parameters
    childNodes ++= method.parameters.importing.zipWithIndex.map { case (param, idx) =>
      createParameterIn(param, idx + 1)
    }

    // Create EXPORTING parameters
    childNodes ++= method.parameters.exporting.zipWithIndex.map { case (param, idx) =>
      createParameterOut(param, idx + 1)
    }

    // Create CHANGING parameters (both IN and OUT)
    method.parameters.changing.zipWithIndex.foreach { case (param, idx) =>
      childNodes :+= createParameterIn(param, idx + 100)
      childNodes :+= createParameterOut(param, idx + 100)
    }

    // Create METHOD_RETURN node (required by CPG schema for ALL methods)
    val methodReturn = method.parameters.returning match {
      case Some(returnParam) =>
        // Method has explicit RETURNING parameter
        createMethodReturn(returnParam)
      case None =>
        // Method has no RETURNING - create void return
        NewMethodReturn()
          .evaluationStrategy(EvaluationStrategies.BY_REFERENCE)
          .typeFullName("void")
          .code("void")
          .order(0)
    }
    childNodes :+= methodReturn

    // Create BLOCK node for method body (required by CPG schema, even if empty)
    val blockNode = NewBlock()
      .order(1)
      .typeFullName("void")

    // Process method body statements (calls, assignments, etc.)
    val blockChildren = method.body match {
      case Some(stmtList) =>
        createStatementsFromBody(stmtList, className)
      case None =>
        Seq.empty
    }

    (methodNode, childNodes, (blockNode, blockChildren))
  }

  /** Create CPG nodes from method body statements */
  private def createStatementsFromBody(stmtList: AbapIntermediateAst.StatementList, className: Option[String]): Seq[NodeWithChildren] = {
    var nodes = Seq[NodeWithChildren]()
    var order = 1

    stmtList.statements.foreach {
      case callExpr: AbapIntermediateAst.CallExpr =>
        nodes :+= createCallNode(callExpr, order, className)
        order += 1

      case assignStmt: AbapIntermediateAst.AssignmentStmt =>
        val nodeWithChildren = createAssignmentCallWithChildren(assignStmt, order, className)
        nodes :+= nodeWithChildren
        order += 1

      case opCall: AbapIntermediateAst.OperatorCall =>
        // Direct operator call (e.g., indirection, field access)
        val nodeWithChildren = createOperatorCallNodeWithChildren(opCall, order, className)
        nodes :+= nodeWithChildren
        order += 1

      case dataDecl: AbapIntermediateAst.DataDeclaration =>
        // Local variable declaration
        val localNode = createLocalNode(dataDecl, order)
        nodes :+= NodeWithChildren(localNode)
        order += 1

        // If there's an initial value, create an assignment: var = value
        dataDecl.initialValue.foreach { initValue =>
          val assignment = AbapIntermediateAst.AssignmentStmt(
            target = AbapIntermediateAst.IdentifierExpr(dataDecl.name, dataDecl.span),
            value = initValue,
            span = dataDecl.span
          )
          val assignmentNode = createAssignmentCallWithChildren(assignment, order, className)
          nodes :+= assignmentNode
          order += 1
        }

      case nestedStmtList: AbapIntermediateAst.StatementList =>
        // Handle chained calls (StatementList containing multiple CallExpr)
        nestedStmtList.statements.foreach {
          case callExpr: AbapIntermediateAst.CallExpr =>
            nodes :+= createCallNode(callExpr, order, className)
            order += 1
          case _ => ()
        }

      case _: AbapIntermediateAst.UnknownNode =>
        // Skip unknown nodes for now
        ()

      case _ =>
        // Skip other node types for now
        ()
    }

    nodes
  }

  /** Create assignment CALL node with ARGUMENT children */
  private def createAssignmentCallWithChildren(
    assignStmt: AbapIntermediateAst.AssignmentStmt,
    order: Int,
    className: Option[String]
  ): NodeWithChildren = {
    // Create the assignment CALL node
    val callNode = NewCall()
      .name("<operator>.assignment")
      .code(s"${codeFromExpr(assignStmt.target)} = ${codeFromExpr(assignStmt.value)}")
      .methodFullName("<operator>.assignment")
      .typeFullName("ANY")
      .dispatchType("STATIC_DISPATCH")
      .order(order)

    assignStmt.span.start.foreach { pos =>
      callNode.lineNumber(pos.row)
      callNode.columnNumber(pos.col)
    }

    // Create children: expressions with argumentIndex set
    var children = Seq[NodeWithChildren]()

    // Argument 1: left side (target)
    val leftExprNode = createExpressionNodeWithChildren(assignStmt.target, 1, className)
    setArgumentIndex(leftExprNode.node, 1)
    children :+= leftExprNode

    // Argument 2: right side (value)
    val rightExprNode = createExpressionNodeWithChildren(assignStmt.value, 2, className)
    setArgumentIndex(rightExprNode.node, 2)
    children :+= rightExprNode

    NodeWithChildren(callNode, children)
  }

  /** Create CPG nodes for an expression (may return CALL, IDENTIFIER, LITERAL, etc.) */
  private def createExpressionNodeWithChildren(expr: AbapIntermediateAst.AbapNode, order: Int, className: Option[String] = None): NodeWithChildren = {
    expr match {
      case ident: AbapIntermediateAst.IdentifierExpr =>
        val identNode = NewIdentifier()
          .name(ident.name)
          .code(ident.name)
          .typeFullName("ANY")
          .order(order)

        ident.span.start.foreach { pos =>
          identNode.lineNumber(pos.row)
          identNode.columnNumber(pos.col)
        }

        NodeWithChildren(identNode)

      case lit: AbapIntermediateAst.LiteralExpr =>
        val litNode = NewLiteral()
          .code(lit.value)
          .typeFullName(lit.literalType)
          .order(order)

        lit.span.start.foreach { pos =>
          litNode.lineNumber(pos.row)
          litNode.columnNumber(pos.col)
        }

        NodeWithChildren(litNode)

      case callExpr: AbapIntermediateAst.CallExpr =>
        createCallNode(callExpr, order, className)

      case opCall: AbapIntermediateAst.OperatorCall =>
        createOperatorCallNodeWithChildren(opCall, order, className)

      case fieldAccess: AbapIntermediateAst.FieldAccessExpr =>
        createFieldAccessNodeWithChildren(fieldAccess, order)

      case stmtList: AbapIntermediateAst.StatementList =>
        // Chained call - take the last call in the chain as the result
        if (stmtList.statements.nonEmpty) {
          val lastCall = stmtList.statements.last
          createExpressionNodeWithChildren(lastCall, order)
        } else {
          val unknownNode = NewIdentifier()
            .name("UNKNOWN")
            .code("UNKNOWN")
            .typeFullName("ANY")
            .order(order)
          NodeWithChildren(unknownNode)
        }

      case _ =>
        // Fallback: create identifier
        val unknownNode = NewIdentifier()
          .name("UNKNOWN")
          .code("UNKNOWN")
          .typeFullName("ANY")
          .order(order)
        NodeWithChildren(unknownNode)
    }
  }

  /** Create CALL node for an operator (e.g., addition, subtraction) */
  private def createOperatorCallNodeWithChildren(
    opCall: AbapIntermediateAst.OperatorCall,
    order: Int,
    className: Option[String] = None
  ): NodeWithChildren = {
    val callNode = NewCall()
      .name(opCall.operatorName)
      .code(s"${opCall.arguments.map(codeFromExpr).mkString(" ")}") // Simplified
      .methodFullName(opCall.operatorName)
      .typeFullName("ANY")
      .dispatchType("STATIC_DISPATCH")
      .order(order)

    opCall.span.start.foreach { pos =>
      callNode.lineNumber(pos.row)
      callNode.columnNumber(pos.col)
    }

    // Create children: expressions with argumentIndex set
    var children = Seq[NodeWithChildren]()
    var argIndex = 1

    opCall.arguments.foreach { argExpr =>
      val exprNode = createExpressionNodeWithChildren(argExpr, argIndex, className)
      setArgumentIndex(exprNode.node, argIndex)
      children :+= exprNode
      argIndex += 1
    }

    NodeWithChildren(callNode, children)
  }

  /** Create CALL node for field access */
  private def createFieldAccessNodeWithChildren(
    fieldAccess: AbapIntermediateAst.FieldAccessExpr,
    order: Int
  ): NodeWithChildren = {
    val callNode = NewCall()
      .name("<operator>.fieldAccess")
      .code(s"${codeFromExpr(fieldAccess.target)}-${fieldAccess.fieldName}")
      .methodFullName("<operator>.fieldAccess")
      .typeFullName("ANY")
      .dispatchType("STATIC_DISPATCH")
      .order(order)

    fieldAccess.span.start.foreach { pos =>
      callNode.lineNumber(pos.row)
      callNode.columnNumber(pos.col)
    }

    // Create children: expressions with argumentIndex set
    var children = Seq[NodeWithChildren]()

    // Argument 1: target expression
    val targetExprNode = createExpressionNodeWithChildren(fieldAccess.target, 1)
    setArgumentIndex(targetExprNode.node, 1)
    children :+= targetExprNode

    // Argument 2: field identifier
    val fieldIdentNode = NewFieldIdentifier()
      .canonicalName(fieldAccess.fieldName)
      .code(fieldAccess.fieldName)
      .argumentIndex(2)
      .order(2)
    children :+= NodeWithChildren(fieldIdentNode)

    NodeWithChildren(callNode, children)
  }

  /** Set argumentIndex on an expression node */
  private def setArgumentIndex(node: NewNode, index: Int): Unit = {
    node match {
      case expr: ExpressionNew => expr.argumentIndex = index
      case _ => () // Other nodes don't have argumentIndex
    }
  }

  /** Get code string from expression */
  private def codeFromExpr(expr: AbapIntermediateAst.AbapNode): String = {
    expr match {
      case ident: AbapIntermediateAst.IdentifierExpr => ident.name
      case lit: AbapIntermediateAst.LiteralExpr => lit.value
      case call: AbapIntermediateAst.CallExpr =>
        call.methodName match {
          case Some(method) =>
            val arrow = if (call.isStatic) "=>" else "->"
            s"${call.targetName}${arrow}${method}()"
          case None => s"${call.targetName}()"
        }
      case fieldAccess: AbapIntermediateAst.FieldAccessExpr =>
        s"${codeFromExpr(fieldAccess.target)}-${fieldAccess.fieldName}"
      case _ => "?"
    }
  }

  /** Create CALL node with ARGUMENT children from CallExpr */
  private def createCallNode(callExpr: AbapIntermediateAst.CallExpr, order: Int, className: Option[String] = None): NodeWithChildren = {
    val methodFullName = (callExpr.targetName.isEmpty, callExpr.methodName) match {
      case (true, Some(method)) =>
        // Simple function call: qualify with class name if available
        className match {
          case Some(cls) => s"$cls::$method"
          case None => method
        }
      case (false, Some(method)) =>
        // Object/class method call: target.method or target=>method
        s"${callExpr.targetName}.${method}"
      case (_, None) =>
        // No method name, use target
        callExpr.targetName
    }

    val code = (callExpr.targetName.isEmpty, callExpr.methodName) match {
      case (true, Some(method)) =>
        // Simple function call
        s"${method}()"
      case (false, Some(method)) =>
        // Object/class method call
        val arrow = if (callExpr.isStatic) "=>" else "->"
        s"${callExpr.targetName}${arrow}${method}()"
      case (_, None) =>
        s"${callExpr.targetName}()"
    }

    val callNode = NewCall()
      .name(callExpr.methodName.getOrElse(callExpr.targetName))
      .code(code)
      .methodFullName(methodFullName)
      .typeFullName("ANY")  // We don't have type information yet
      .dispatchType(if (callExpr.isStatic) "STATIC_DISPATCH" else "DYNAMIC_DISPATCH")
      .order(order)

    // Add line/column numbers if available
    callExpr.span.start.foreach { pos =>
      callNode.lineNumber(pos.row)
      callNode.columnNumber(pos.col)
    }

    // Create ARGUMENT children from call arguments
    var children = Seq[NodeWithChildren]()
    var argIndex = 1

    callExpr.arguments.foreach { arg =>
      // Create expression node for the argument value
      val argExprNode = createExpressionNodeWithChildren(arg.value, argIndex, className)
      setArgumentIndex(argExprNode.node, argIndex)

      // If the argument has a name (named parameter), store it
      arg.name.foreach { paramName =>
        argExprNode.node match {
          case identifier: NewIdentifier =>
            // For named parameters, we might want to track the parameter name
            // For now, just use argumentIndex
            ()
          case _ => ()
        }
      }

      children :+= argExprNode
      argIndex += 1
    }

    NodeWithChildren(callNode, children)
  }

  /** Create LOCAL node for variable declaration */
  private def createLocalNode(dataDecl: AbapIntermediateAst.DataDeclaration, order: Int): NewLocal = {
    // Generate code representation
    val codeStr = dataDecl.initialValue match {
      case Some(initVal) =>
        val initCode = codeFromExpr(initVal)
        s"DATA: ${dataDecl.name} TYPE ${dataDecl.typeName} VALUE ${initCode}."
      case None =>
        s"DATA: ${dataDecl.name} TYPE ${dataDecl.typeName}."
    }

    val localNode = NewLocal()
      .name(dataDecl.name)
      .code(codeStr)
      .typeFullName(dataDecl.typeName)
      .order(order)

    // Add line/column numbers if available
    dataDecl.span.start.foreach { pos =>
      localNode.lineNumber(pos.row)
      localNode.columnNumber(pos.col)
    }

    localNode
  }

  /** Create METHOD_PARAMETER_IN node */
  private def createParameterIn(param: Parameter, index: Int): NewMethodParameterIn = {
    val evaluationStrategy = if (param.isValue) {
      EvaluationStrategies.BY_VALUE
    } else {
      EvaluationStrategies.BY_REFERENCE
    }

    NewMethodParameterIn()
      .name(param.name)
      .code(param.name)
      .typeFullName(param.typeName)
      .evaluationStrategy(evaluationStrategy)
      .index(index)
      .order(index)
  }

  /** Create METHOD_PARAMETER_OUT node */
  private def createParameterOut(param: Parameter, index: Int): NewMethodParameterOut = {
    val evaluationStrategy = if (param.isValue) {
      EvaluationStrategies.BY_VALUE
    } else {
      EvaluationStrategies.BY_REFERENCE
    }

    NewMethodParameterOut()
      .name(param.name)
      .code(param.name)
      .typeFullName(param.typeName)
      .evaluationStrategy(evaluationStrategy)
      .index(index)
      .order(index)
  }

  /** Create METHOD_RETURN node */
  private def createMethodReturn(returnParam: Parameter): NewMethodReturn = {
    val evaluationStrategy = if (returnParam.isValue) {
      EvaluationStrategies.BY_VALUE
    } else {
      EvaluationStrategies.BY_REFERENCE
    }

    NewMethodReturn()
      .evaluationStrategy(evaluationStrategy)
      .typeFullName(returnParam.typeName)
      .code(returnParam.name)
      .order(0)
  }

  /** Create method signature string */
  private def createSignature(params: MethodParameters): String = {
    val importingTypes = params.importing.map(_.typeName).mkString(", ")
    val exportingTypes = params.exporting.map(_.typeName).mkString(", ")
    val changingTypes = params.changing.map(_.typeName).mkString(", ")

    val returnType = params.returning.map(_.typeName).getOrElse("void")

    val allParams = Seq(importingTypes, exportingTypes, changingTypes)
      .filter(_.nonEmpty)
      .mkString("; ")

    if (allParams.nonEmpty) {
      s"($allParams) -> $returnType"
    } else {
      s"() -> $returnType"
    }
  }

  /** Recursively collect all CALL nodes from a statement tree */
  private def collectCallNodes(
    nodeWithChildren: NodeWithChildren,
    className: Option[String],
    accumulator: scala.collection.mutable.ArrayBuffer[(NewCall, Option[String])]
  ): Unit = {
    // Check if this node is a CALL
    nodeWithChildren.node match {
      case call: NewCall =>
        // Don't track operator calls for CALL edges
        if (!call.name.startsWith("<operator>")) {
          accumulator += ((call, className))
        }
      case _ => ()
    }

    // Recursively process children
    nodeWithChildren.children.foreach { child =>
      collectCallNodes(child, className, accumulator)
    }
  }

  /** Add CALL edges from CALL nodes to their target METHOD nodes */
  private def addCallEdges(
    diffGraph: DiffGraphBuilder,
    allMethods: Seq[NewMethod],
    allCalls: Seq[(NewCall, Option[String])]
  ): Unit = {
    // Build method index: fullName -> METHOD
    val methodIndex = allMethods.map(m => m.fullName -> m).toMap

    // Also index by simple name for unqualified calls
    val methodByName = allMethods.groupBy(_.name)

    allCalls.foreach { case (call, callerClass) =>
      // Try to resolve the target method
      // For ABAP: static calls have format "class=>method" in methodFullName
      // Instance calls have format "object->method"

      val targetMethod = call.methodFullName match {
        case fullName if fullName.contains("::") =>
          // Fully qualified: "ClassName::methodName"
          methodIndex.get(fullName)

        case fullName if fullName.contains("=>") =>
          // Static call: try to parse "class=>method"
          val parts = fullName.split("=>")
          if (parts.length == 2) {
            val className = parts(0)
            val methodName = parts(1).takeWhile(_ != '(') // Remove arguments
            methodIndex.get(s"$className::$methodName")
          } else {
            None
          }

        case methodName =>
          // Unqualified call - try to find method in same class first
          callerClass match {
            case Some(cls) =>
              // Try same class first
              methodIndex.get(s"$cls::$methodName")
                .orElse(methodByName.get(methodName).flatMap(_.headOption))
            case None =>
              // Standalone - just find by name
              methodByName.get(methodName).flatMap(_.headOption)
          }
      }

      // Add CALL edge if we found a target
      targetMethod.foreach { method =>
        diffGraph.addEdge(call, method, EdgeTypes.CALL)
      }
    }
  }

  /** Add CFG edges between statements in a method */
  private def addCfgEdges(
    diffGraph: DiffGraphBuilder,
    methodNode: NewMethod,
    methodChildren: Seq[NewNode],
    blockChildren: Seq[NodeWithChildren]
  ): Unit = {
    // Find METHOD_RETURN node
    val methodReturnOpt = methodChildren.collectFirst {
      case mr: NewMethodReturn => mr
    }

    // Get ONLY CFG nodes from blockChildren (exclude LOCAL declarations)
    // LOCAL nodes are AST-only, not part of the control flow
    val cfgStatements = blockChildren.map(_.node).filterNot {
      case _: NewLocal => true  // Skip LOCAL nodes - they're not CfgNodes
      case _ => false
    }

    if (cfgStatements.nonEmpty) {
      // CFG edge from METHOD to first CFG statement (skip LOCAL declarations)
      diffGraph.addEdge(methodNode, cfgStatements.head, EdgeTypes.CFG)

      // CFG edges between consecutive CFG statements (only if there are 2+ statements)
      if (cfgStatements.size > 1) {
        cfgStatements.sliding(2).foreach {
          case Seq(prev, next) =>
            diffGraph.addEdge(prev, next, EdgeTypes.CFG)
          case _ => () // Handle edge case
        }
      }

      // CFG edge from last CFG statement to METHOD_RETURN (if exists)
      methodReturnOpt.foreach { methodReturn =>
        diffGraph.addEdge(cfgStatements.last, methodReturn, EdgeTypes.CFG)
      }
    } else {
      // Empty method body - direct CFG edge from METHOD to METHOD_RETURN
      methodReturnOpt.foreach { methodReturn =>
        diffGraph.addEdge(methodNode, methodReturn, EdgeTypes.CFG)
      }
    }
  }

}

object CpgGenerator {
  def apply(): CpgGenerator = new CpgGenerator()
}
