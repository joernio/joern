package io.shiftleft.py2cpg

import io.shiftleft.codepropertygraph.generated.nodes
import io.shiftleft.codepropertygraph.generated.nodes.{
  NewClosureBinding,
  NewIdentifier,
  NewLocal,
  NewMethod,
  NewNode
}
import io.shiftleft.py2cpg.memop._
import org.slf4j.LoggerFactory

import scala.collection.mutable

object ContextStack {
  private val logger = LoggerFactory.getLogger(getClass)
}

class ContextStack {
  import ContextStack.logger

  private sealed trait ContextType
  private object MethodContext extends ContextType
  private object ClassContext extends ContextType

  private class Context(
      val typ: ContextType,
      val name: String,
      val cpgNode: nodes.NewNode,
      val order: AutoIncIndex,
      val methodBlockNode: Option[nodes.NewBlock] = None,
      val methodRefNode: Option[nodes.NewMethodRef] = None,
      val variables: mutable.Map[String, nodes.NewNode] = mutable.Map.empty,
      val globalVariables: mutable.Set[String] = mutable.Set.empty,
      val nonLocalVariables: mutable.Set[String] = mutable.Set.empty
  ) {}

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

  private var stack = List[Context]()
  private val variableReferences = mutable.ArrayBuffer.empty[VariableReference]
  private var moduleMethodContext = Option.empty[Context]
  private var fileNamespaceBlock = Option.empty[nodes.NewNamespaceBlock]
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
    val methodContext = new Context(
      MethodContext,
      name,
      methodNode,
      new AutoIncIndex(1),
      Some(methodBlockNode),
      methodRefNode
    )
    if (moduleMethodContext.isEmpty) {
      moduleMethodContext = Some(methodContext)
    }
    push(methodContext)
  }

  def pushClass(name: String, classNode: nodes.NewTypeDecl): Unit = {
    push(new Context(ClassContext, name, classNode, new AutoIncIndex(1)))
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

  private def findEnclosingMethodContext(contextStack: List[Context]): Context = {
    contextStack.find(_.typ == MethodContext).get
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
    // referencing the global variable which are syntactially before the store
    // operations.
    variableReferences.foreach { case VariableReference(identifier, memOp, stack) =>
      val name = identifier.name
      if (
        memOp == Store &&
        stack.head.globalVariables.contains(name) &&
        !moduleMethodContext.get.variables.contains(name)
      ) {
        val localNode = createLocal(name, None)
        createAstEdge(
          localNode,
          moduleMethodContext.get.methodBlockNode.get,
          moduleMethodContext.get.order.getAndInc
        )
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
      memOp match {
        case Load =>
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
        case _
            if contextStack.head.globalVariables.contains(
              name
            ) || contextStack.head.nonLocalVariables.contains(name) =>
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
        case Store =>
          var variableNode = lookupVariableInMethodContext(name, contextStack)
          if (variableNode.isEmpty) {
            val localNode = createLocal(name, None)
            val enclosingMethodContext = findEnclosingMethodContext(contextStack)
            createAstEdge(
              localNode,
              enclosingMethodContext.methodBlockNode.get,
              enclosingMethodContext.order.getAndInc
            )
            enclosingMethodContext.variables.put(name, localNode)
            variableNode = Some(localNode)
          }
          createRefEdge(variableNode.get, identifier)
        case Del =>
          val variableNode = lookupVariableInMethodContext(name, contextStack)
          variableNode match {
            case Some(variableNode) =>
              createRefEdge(variableNode, identifier)
            case None =>
              logger.warn("Unable to link identifier. Resulting CPG will have invalid format.")
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
  ) = {
    var identifierOrClosureBindingToLink: nodes.NewNode = identifier
    val stackIt = contextStack.iterator
    var contextHasVariable = false
    while (stackIt.hasNext && !contextHasVariable) {
      val context = stackIt.next
      contextHasVariable = context.variables.contains(name)
      val closureBindingId = context.cpgNode.asInstanceOf[NewMethod].fullName + ":" + name

      if (!contextHasVariable) {
        if (context != moduleMethodContext.get) {
          val localNode = createLocal(name, Some(closureBindingId))
          createAstEdge(localNode, context.methodBlockNode.get, context.order.getAndInc)
          context.variables.put(name, localNode)
        } else {
          // When we could not even find a matching variable in the module context we get
          // here and create a local so that we can link something and fullfil the CPG
          // format requirements.
          // For example this happens when there are wildcard imports directly into the
          // modules namespace.
          val localNode = createLocal(name, None)
          createAstEdge(localNode, context.methodBlockNode.get, context.order.getAndInc)
          context.variables.put(name, localNode)
        }
      }
      val localNodeInContext = context.variables.get(name).get

      createRefEdge(localNodeInContext, identifierOrClosureBindingToLink)

      if (!contextHasVariable && context != moduleMethodContext.get) {
        identifierOrClosureBindingToLink = createClosureBinding(closureBindingId, name)
        createCaptureEdge(identifierOrClosureBindingToLink, context.methodRefNode.get)
      }
    }
  }

  private def lookupVariableInMethodContext(
      name: String,
      stack: List[Context]
  ): Option[nodes.NewNode] = {
    var variableNode = Option.empty[nodes.NewNode]

    val stackIt = stack.iterator
    var lastContextWasMethod = false
    while (stackIt.hasNext && variableNode.isEmpty && !lastContextWasMethod) {
      val context = stackIt.next
      variableNode = context.variables.get(name)
      lastContextWasMethod = context.typ == MethodContext
    }
    variableNode
  }

  def addParameter(parameter: nodes.NewMethodParameterIn): Unit = {
    assert(stack.head.typ == MethodContext)
    stack.head.variables.put(parameter.name, parameter)
  }

  def addGlobalVariable(name: String): Unit = {
    stack.head.globalVariables.add(name)
  }

  def addNonLocalVariable(name: String): Unit = {
    stack.head.nonLocalVariables.add(name)
  }

  // Together with the file name this is used to compute full names.
  def qualName: String = {
    stack
      .filter { element =>
        element.typ == MethodContext || element.typ == ClassContext
      }
      .map(_.name)
      .reverse
      .mkString(".")
  }

  def astParent: nodes.NewNode = {
    stack match {
      case head :: _ =>
        head.cpgNode
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
    stack.head.typ == ClassContext
  }

}
