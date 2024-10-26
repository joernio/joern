package io.joern.pysrc2cpg

import io.joern.pysrc2cpg.ContextStack.transferLineColInfo
import io.joern.pysrc2cpg.memop.*
import io.joern.x2cpg.frontendspecific.pysrc2cpg.Constants
import io.shiftleft.codepropertygraph.generated.nodes
import io.shiftleft.codepropertygraph.generated.nodes.*
import org.slf4j.LoggerFactory

import scala.collection.mutable

object ContextStack {
  private val logger = LoggerFactory.getLogger(getClass)

  def transferLineColInfo(src: NewIdentifier, tgt: NewLocal): Unit = {
    src.lineNumber match {
      // If there are multiple occurrences and the local is already set, ignore later updates
      case Some(srcLineNo) if tgt.lineNumber.isEmpty || !tgt.lineNumber.exists(_ < srcLineNo) =>
        tgt.lineNumber(src.lineNumber)
        tgt.columnNumber(src.columnNumber)
      case _ =>
    }
  }
}

class ContextStack {
  import ContextStack.logger

  private trait Context {
    val astParent: nodes.NewNode
    val order: AutoIncIndex
    val variables: mutable.Map[String, nodes.NewNode]
    var lambdaCounter: Int
  }

  private class MethodContext(
    val scopeName: Option[String],
    val astParent: nodes.NewNode,
    val order: AutoIncIndex,
    val isClassBodyMethod: Boolean = false,
    val methodBlockNode: Option[nodes.NewBlock] = None,
    val methodRefNode: Option[nodes.NewMethodRef] = None,
    val variables: mutable.Map[String, nodes.NewNode] = mutable.Map.empty,
    val globalVariables: mutable.Set[String] = mutable.Set.empty,
    val nonLocalVariables: mutable.Set[String] = mutable.Set.empty,
    var lambdaCounter: Int = 0
  ) extends Context {}

  private class ClassContext(
    val scopeName: Option[String],
    val astParent: nodes.NewNode,
    val order: AutoIncIndex,
    val variables: mutable.Map[String, nodes.NewNode] = mutable.Map.empty,
    var lambdaCounter: Int = 0
  ) extends Context {}

  // Used to represent comprehension variable and exception
  // handler context.
  // E.g.: [x for x in y] creates an extra context with a
  // local variable x which is different from a possible x
  // in the surrounding method context. The same applies
  // to x in:
  // try:
  //   pass
  // except e as x:
  //   pass
  private class SpecialBlockContext(
    val astParent: nodes.NewNode,
    val order: AutoIncIndex,
    val variables: mutable.Map[String, nodes.NewNode] = mutable.Map.empty,
    var lambdaCounter: Int = 0
  ) extends Context {}

  private case class VariableReference(
    identifier: nodes.NewIdentifier,
    memOp: MemoryOperation,
    // Context stack as it was when VariableReference
    // was created. Context objects are and need to
    // shared between different VariableReference
    // instances because the changes in the variable
    // maps need to be in sync.
    stack: List[Context]
  )

  private var stack                   = List[Context]()
  private val variableReferences      = mutable.ArrayBuffer.empty[VariableReference]
  private var moduleMethodContext     = Option.empty[MethodContext]
  private var fileNamespaceBlock      = Option.empty[nodes.NewNamespaceBlock]
  private val fileNamespaceBlockOrder = new AutoIncIndex(1)

  private def push(context: Context): Unit = {
    stack = context :: stack
  }

  def pushMethod(
    scopeName: Option[String],
    methodNode: nodes.NewMethod,
    methodBlockNode: nodes.NewBlock,
    methodRefNode: Option[nodes.NewMethodRef]
  ): Unit = {
    val isClassBodyMethod = stack.headOption.exists(_.isInstanceOf[ClassContext])

    val methodContext =
      new MethodContext(
        scopeName,
        methodNode,
        new AutoIncIndex(1),
        isClassBodyMethod,
        Some(methodBlockNode),
        methodRefNode
      )
    if (moduleMethodContext.isEmpty) {
      moduleMethodContext = Some(methodContext)
    }
    push(methodContext)
  }

  def pushClass(scopeName: Option[String], classNode: nodes.NewTypeDecl): Unit = {
    push(new ClassContext(scopeName, classNode, new AutoIncIndex(1)))
  }

  def pushSpecialContext(): Unit = {
    val methodContext = findEnclosingMethodContext(stack)
    push(new SpecialBlockContext(methodContext.astParent, methodContext.order))
  }

  def pop(): Unit = {
    stack = stack.tail
  }

  def setFileNamespaceBlock(namespaceBlock: nodes.NewNamespaceBlock): Unit = {
    fileNamespaceBlock = Some(namespaceBlock)
  }

  def addVariableReference(identifier: nodes.NewIdentifier, memOp: MemoryOperation): Unit = {
    variableReferences.append(VariableReference(identifier, memOp, stack))
  }

  def getAndIncLambdaCounter(): Int = {
    val result = stack.head.lambdaCounter
    stack.head.lambdaCounter += 1
    result
  }

  private def findEnclosingMethodContext(contextStack: List[Context]): MethodContext = {
    contextStack.find(_.isInstanceOf[MethodContext]).get.asInstanceOf[MethodContext]
  }

  def findEnclosingTypeDecl(): Option[NewNode] = {
    stack.find(_.isInstanceOf[ClassContext]) match {
      case Some(classContext: ClassContext) =>
        Some(classContext.astParent)
      case _ => None
    }
  }

  def createIdentifierLinks(
    createLocal: (String, Option[String]) => nodes.NewLocal,
    createClosureBinding: (String, String) => nodes.NewClosureBinding,
    createAstEdge: (nodes.NewNode, nodes.NewNode, Int) => Unit,
    createRefEdge: (nodes.NewNode, nodes.NewNode) => Unit,
    createCaptureEdge: (nodes.NewNode, nodes.NewNode) => Unit
  ): Unit = {
    // Before we do any linking, we iterate over all variable references and
    // create a variable in the module method context for each global variable
    // with a store operation on it.
    // This is necessary because there might be load/delete operations
    // referencing the global variable which are syntactically before the store
    // operations.
    variableReferences.foreach { case VariableReference(identifier, memOp, contextStack) =>
      val name = identifier.name
      if (
        memOp == Store &&
        findEnclosingMethodContext(contextStack).globalVariables.contains(name) &&
        !moduleMethodContext.get.variables.contains(name)
      ) {
        val localNode = createLocal(name, None)
        transferLineColInfo(identifier, localNode)
        createAstEdge(localNode, moduleMethodContext.get.methodBlockNode.get, moduleMethodContext.get.order.getAndInc)
        moduleMethodContext.get.variables.put(name, localNode)
      }
    }

    // Variable references processing needs to be ordered by context depth in
    // order to make sure that variables captured into deeper nested contexts
    // are already created.
    val sortedVariableRefs = variableReferences.sortBy(_.stack.size)
    sortedVariableRefs.foreach { case VariableReference(identifier, memOp, contextStack) =>
      val name = identifier.name
      // Store and delete operations look up variable only in method scope.
      // Load operations also look up captured or global variables.
      // If a store and load/del happens in the same context, the store must
      // come first. Otherwise it is not valid Python, which we assume here.
      if (memOp == Load) {
        linkLocalOrCapturing(
          createLocal,
          createClosureBinding,
          createAstEdge,
          createRefEdge,
          createCaptureEdge,
          identifier,
          name,
          contextStack
        )
      } else {
        val enclosingMethodContext = findEnclosingMethodContext(contextStack)

        if (
          enclosingMethodContext.globalVariables.contains(name) ||
          enclosingMethodContext.nonLocalVariables.contains(name)
        ) {
          linkLocalOrCapturing(
            createLocal,
            createClosureBinding,
            createAstEdge,
            createRefEdge,
            createCaptureEdge,
            identifier,
            name,
            contextStack
          )
        } else if (memOp == Store) {
          var variableNode = lookupVariableInMethod(name, contextStack)
          if (variableNode.isEmpty) {
            val localNode = createLocal(name, None)
            transferLineColInfo(identifier, localNode)
            val enclosingMethodContext = findEnclosingMethodContext(contextStack)
            createAstEdge(localNode, enclosingMethodContext.methodBlockNode.get, enclosingMethodContext.order.getAndInc)
            enclosingMethodContext.variables.put(name, localNode)
            variableNode = Some(localNode)
          }
          createRefEdge(variableNode.get, identifier)
        } else if (memOp == Del) {
          val variableNode = lookupVariableInMethod(name, contextStack)
          variableNode match {
            case Some(variableNode) =>
              createRefEdge(variableNode, identifier)
            case None =>
              // When we could not find a matching variable we get here and create a local in
              // the method context so that we can link something and fullfil the CPG
              // format requirements.
              // For example this happens when there are wildcard imports directly into the
              // modules namespace.
              val localNode = createLocal(name, None)
              transferLineColInfo(identifier, localNode)
              val methodContext = findEnclosingMethodContext(contextStack)
              createAstEdge(localNode, methodContext.methodBlockNode.get, methodContext.order.getAndInc)
              methodContext.variables.put(name, localNode)
              createRefEdge(localNode, identifier)
          }
        }
      }
    }
  }

  /** Assignments to variables on the module-level may be exported to other modules and behave as inter-procedurally
    * global variables.
    * @param lhs
    *   the LHS node of an assignment
    */
  def considerAsGlobalVariable(lhs: NewNode): Unit = {
    lhs match {
      case n: NewIdentifier if findEnclosingMethodContext(stack).scopeName.contains(Constants.moduleName) =>
        addGlobalVariable(n.name)
      case _ =>
    }
  }

  /** For module-methods, the variables of this method can be imported into other modules which resembles behaviour much
    * like fields/members. This inter-procedural accessibility should be marked via the module's type decl node.
    */
  def createMemberLinks(moduleTypeDecl: NewTypeDecl, astEdgeLinker: (NewNode, NewNode, Int) => Unit): Unit = {
    val globalVarsForEnclMethod = findEnclosingMethodContext(stack).globalVariables
    variableReferences
      .map(_.identifier)
      .filter(i => globalVarsForEnclMethod.contains(i.name))
      .sortBy(i => (i.lineNumber, i.columnNumber))
      .distinctBy(_.name)
      .map(i =>
        NewMember()
          .name(i.name)
          .typeFullName(Constants.ANY)
          .dynamicTypeHintFullName(i.dynamicTypeHintFullName)
          .lineNumber(i.lineNumber)
          .columnNumber(i.columnNumber)
          .code(i.name)
      )
      .zipWithIndex
      .foreach { case (m, idx) => astEdgeLinker(m, moduleTypeDecl, idx + 1) }
  }

  private def linkLocalOrCapturing(
    createLocal: (String, Option[String]) => NewLocal,
    createClosureBinding: (String, String) => NewClosureBinding,
    createAstEdge: (NewNode, NewNode, Int) => Unit,
    createRefEdge: (NewNode, NewNode) => Unit,
    createCaptureEdge: (NewNode, NewNode) => Unit,
    identifier: NewIdentifier,
    name: String,
    contextStack: List[Context]
  ): Unit = {
    var identifierOrClosureBindingToLink: nodes.NewNode = identifier
    val stackIt                                         = contextStack.iterator
    var contextHasVariable                              = false
    val startContext                                    = contextStack.head
    while (stackIt.hasNext && !contextHasVariable) {
      val context = stackIt.next()

      context match {
        case methodContext: MethodContext =>
          // Context is only relevant for linking if it is not a class body methods context
          // or the identifier/reference itself is from the class body method context.
          if (!methodContext.isClassBodyMethod || methodContext == startContext) {
            contextHasVariable = context.variables.contains(name)

            val closureBindingId =
              methodContext.astParent.asInstanceOf[NewMethod].fullName + ":" + name

            if (!contextHasVariable) {
              if (context != moduleMethodContext.get) {
                val localNode = createLocal(name, Some(closureBindingId))
                transferLineColInfo(identifier, localNode)
                createAstEdge(localNode, methodContext.methodBlockNode.get, methodContext.order.getAndInc)
                methodContext.variables.put(name, localNode)
              } else {
                // When we could not even find a matching variable in the module context we get
                // here and create a local so that we can link something and fullfil the CPG
                // format requirements.
                // For example this happens when there are wildcard imports directly into the
                // modules namespace.
                val localNode = createLocal(name, None)
                transferLineColInfo(identifier, localNode)
                createAstEdge(localNode, methodContext.methodBlockNode.get, methodContext.order.getAndInc)
                methodContext.variables.put(name, localNode)
              }
            }
            val localNodeInContext = methodContext.variables(name)

            createRefEdge(localNodeInContext, identifierOrClosureBindingToLink)

            if (!contextHasVariable && context != moduleMethodContext.get) {
              identifierOrClosureBindingToLink = createClosureBinding(closureBindingId, name)
              createCaptureEdge(identifierOrClosureBindingToLink, methodContext.methodRefNode.get)
            }
          }
        case specialBlockContext: SpecialBlockContext =>
          contextHasVariable = context.variables.contains(name)
          if (contextHasVariable) {
            val localNodeInContext = specialBlockContext.variables(name)
            createRefEdge(localNodeInContext, identifierOrClosureBindingToLink)
          }

        case _: ClassContext =>
          assert(context.variables.isEmpty)
        // Class context is not relevant for variable linking.
        // The context relevant for this is the class method body context.
      }
    }
  }

  private def lookupVariableInMethod(name: String, stack: List[Context]): Option[nodes.NewNode] = {
    var variableNode = Option.empty[nodes.NewNode]

    val stackIt              = stack.iterator
    var lastContextWasMethod = false
    while (stackIt.hasNext && variableNode.isEmpty && !lastContextWasMethod) {
      val context = stackIt.next()
      variableNode = context.variables.get(name)
      lastContextWasMethod = context.isInstanceOf[MethodContext]
    }
    variableNode
  }

  def addParameter(parameter: nodes.NewMethodParameterIn): Unit = {
    assert(stack.head.isInstanceOf[MethodContext])
    stack.head.variables.put(parameter.name, parameter)
  }

  def addSpecialVariable(local: nodes.NewLocal): Unit = {
    assert(stack.head.isInstanceOf[SpecialBlockContext])
    stack.head.variables.put(local.name, local)
  }

  def addGlobalVariable(name: String): Unit = {
    findEnclosingMethodContext(stack).globalVariables.add(name)
  }

  def addNonLocalVariable(name: String): Unit = {
    findEnclosingMethodContext(stack).nonLocalVariables.add(name)
  }

  // Together with the file name this is used to compute full names.
  def qualName: String = {
    stack
      .flatMap {
        case methodContext: MethodContext =>
          methodContext.scopeName
        case specialBlockContext: SpecialBlockContext =>
          None
        case classContext: ClassContext =>
          classContext.scopeName
      }
      .reverse
      .mkString(".")
  }

  def astParent: nodes.NewNode = {
    stack match {
      case head :: _ =>
        head.astParent
      case Nil =>
        fileNamespaceBlock.get
    }
  }

  def order: AutoIncIndex = {
    stack match {
      case head :: _ =>
        head.order
      case Nil =>
        fileNamespaceBlockOrder
    }
  }

  def isClassContext: Boolean = {
    stack.nonEmpty && (stack.head match {
      case methodContext: MethodContext if methodContext.isClassBodyMethod => true
      case _                                                               => false
    })
  }

}
