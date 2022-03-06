package io.joern.pysrc2cpg

import io.shiftleft.codepropertygraph.generated.nodes
import io.shiftleft.codepropertygraph.generated.nodes.{NewClosureBinding, NewIdentifier, NewLocal, NewMethod, NewNode}
import io.joern.pysrc2cpg.memop._
import org.slf4j.LoggerFactory

import scala.collection.mutable

object ContextStack {
  private val logger = LoggerFactory.getLogger(getClass)
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
    val name: String,
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
    val name: String,
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
  private var fileNamespaceBlockOrder = new AutoIncIndex(1)

  private def push(context: Context): Unit = {
    stack = context :: stack
  }

  def pushMethod(
    name: String,
    methodNode: nodes.NewMethod,
    methodBlockNode: nodes.NewBlock,
    methodRefNode: Option[nodes.NewMethodRef]
  ): Unit = {
    val isClassBodyMethod = stack.headOption.exists(_.isInstanceOf[ClassContext])

    val methodContext =
      new MethodContext(name, methodNode, new AutoIncIndex(1), isClassBodyMethod, Some(methodBlockNode), methodRefNode)
    if (moduleMethodContext.isEmpty) {
      moduleMethodContext = Some(methodContext)
    }
    push(methodContext)
  }

  def pushClass(name: String, classNode: nodes.NewTypeDecl): Unit = {
    push(new ClassContext(name, classNode, new AutoIncIndex(1)))
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
    variableReferences.append(new VariableReference(identifier, memOp, stack))
  }

  def getAndIncLambdaCounter(): Int = {
    val result = stack.head.lambdaCounter
    stack.head.lambdaCounter += 1
    result
  }

  private def findEnclosingMethodContext(contextStack: List[Context]): MethodContext = {
    contextStack.find(_.isInstanceOf[MethodContext]).get.asInstanceOf[MethodContext]
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
            val localNode              = createLocal(name, None)
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
              logger.warn("Unable to link identifier. Resulting CPG will have invalid format.")
          }

        }
      }
    }
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
                createAstEdge(localNode, methodContext.methodBlockNode.get, methodContext.order.getAndInc)
                methodContext.variables.put(name, localNode)
              } else {
                // When we could not even find a matching variable in the module context we get
                // here and create a local so that we can link something and fullfil the CPG
                // format requirements.
                // For example this happens when there are wildcard imports directly into the
                // modules namespace.
                val localNode = createLocal(name, None)
                createAstEdge(localNode, methodContext.methodBlockNode.get, methodContext.order.getAndInc)
                methodContext.variables.put(name, localNode)
              }
            }
            val localNodeInContext = methodContext.variables.get(name).get

            createRefEdge(localNodeInContext, identifierOrClosureBindingToLink)

            if (!contextHasVariable && context != moduleMethodContext.get) {
              identifierOrClosureBindingToLink = createClosureBinding(closureBindingId, name)
              createCaptureEdge(identifierOrClosureBindingToLink, methodContext.methodRefNode.get)
            }
          }
        case specialBlockContext: SpecialBlockContext =>
          contextHasVariable = context.variables.contains(name)
          if (contextHasVariable) {
            val localNodeInContext = specialBlockContext.variables.get(name).get
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
          Some(methodContext.name)
        case specialBlockContext: SpecialBlockContext =>
          None
        case classContext: ClassContext =>
          Some(classContext.name)
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
    val stackTail = stack.tail
    stackTail.nonEmpty &&
    (stackTail.head.isInstanceOf[ClassContext] ||
      stackTail.head.asInstanceOf[MethodContext].name.endsWith("<body>"))
  }

}
