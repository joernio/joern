package io.joern.rust2cpg.astcreation

import io.joern.x2cpg.Defines
import io.shiftleft.codepropertygraph.generated.nodes.{
  NewIdentifier,
  NewLocal,
  NewMethod,
  NewMethodParameterIn,
  NewNamespaceBlock,
  NewNode,
  NewTypeDecl
}

import scala.annotation.tailrec
import scala.collection.mutable

object ContextStack {

  sealed trait Context

  final class NamespaceContext(val namespaceBlock: NewNamespaceBlock) extends Context
  final class TypeDeclContext(val typeDecl: NewTypeDecl)              extends Context
  final class BlockContext(val items: mutable.Map[String, NewLocal])  extends Context
  final class LocalContext(val local: NewLocal)                       extends Context

  class MethodContext(
    val method: NewMethod,
    val parameters: mutable.Map[String, NewMethodParameterIn],
    var tmpCounter: Int
  ) extends Context {

    def nextTmpName(): String = {
      val name = s"<tmp>$tmpCounter"
      tmpCounter += 1
      name
    }
  }

  final class GlobalMethodContext(method: NewMethod) extends MethodContext(method, mutable.Map.empty, 0)

  @tailrec
  private def rustItemParentFullName(stack: List[Context]): String = stack match {
    case (ctx: GlobalMethodContext) :: tail => rustItemParentFullName(tail)
    case (ctx: MethodContext) :: _          => ctx.method.fullName
    case (ctx: NamespaceContext) :: _       => ctx.namespaceBlock.name
    case (ctx: TypeDeclContext) :: _        => ctx.typeDecl.fullName
    case _ :: tail                          => rustItemParentFullName(tail)
    case _                                  => Defines.UnresolvedNamespace
  }

  @tailrec
  private def parentIsMethod(stack: List[Context]): Boolean = stack match {
    case (_: GlobalMethodContext) :: _ => false
    case (_: MethodContext) :: _       => true
    case (_: NamespaceContext) :: _    => false
    case (_: TypeDeclContext) :: _     => false
    case (_: LocalContext) :: tail     => parentIsMethod(tail)
    case (_: BlockContext) :: tail     => parentIsMethod(tail)
    case Nil                           => false
  }

  @tailrec
  def lookup(name: String, stack: List[Context]): Option[NewLocal | NewMethodParameterIn] = {
    stack match {
      case Nil                         => None
      case (ctx: LocalContext) :: tail => if (ctx.local.name == name) Some(ctx.local) else lookup(name, tail)
      case (ctx: BlockContext) :: tail =>
        ctx.items.get(name) match {
          case Some(decl) => Some(decl)
          case None       => lookup(name, tail)
        }
      case (ctx: MethodContext) :: tail    => ctx.parameters.get(name)
      case (ctx: NamespaceContext) :: tail => None
      case (ctx: TypeDeclContext) :: tail  => None
    }
  }
}

class ContextStack {
  import ContextStack.*

  private var stack              = List.empty[Context]
  private val variableReferences = mutable.ListBuffer.empty[(NewIdentifier, List[Context])]

  private def push(context: Context): Unit = {
    stack = context :: stack
  }

  def pushNamespace(namespaceBlock: NewNamespaceBlock): Unit = {
    push(new NamespaceContext(namespaceBlock))
  }

  def pushTypeDecl(typeDecl: NewTypeDecl): Unit = {
    push(new TypeDeclContext(typeDecl))
  }

  def pushBlock(): Unit = {
    push(new BlockContext(mutable.Map.empty))
  }

  def pushMethod(method: NewMethod): Unit = {
    push(new MethodContext(method, mutable.Map.empty, 0))
  }

  def pushGlobalMethod(method: NewMethod): Unit = {
    push(new GlobalMethodContext(method))
  }

  def pop(): Unit = {
    // LocalContext stays open until its enclosing scope ends.
    // No pushLocal exists on purpose.
    stack = stack.dropWhile(_.isInstanceOf[LocalContext]).tail
  }

  def declareLocal(local: NewLocal): Unit = {
    push(new LocalContext(local))
  }

  def nextTmpName(): String = {
    stack.collectFirst { case method: MethodContext => method }.get.nextTmpName()
  }

  def declareParameter(parameter: NewMethodParameterIn): Unit = {
    val method = stack.collectFirst { case method: MethodContext => method }.get
    method.parameters(parameter.name) = parameter
  }

  def addVariableReference(identifier: NewIdentifier): Unit = {
    variableReferences.append((identifier, stack))
  }

  def astParentType: String = stack.collectFirst {
    case ctx: MethodContext    => ctx.method.label
    case ctx: NamespaceContext => ctx.namespaceBlock.label
    case ctx: TypeDeclContext  => ctx.typeDecl.label
  }.get

  def astParentFullName: String = stack.collectFirst {
    case ctx: MethodContext    => ctx.method.fullName
    case ctx: NamespaceContext => ctx.namespaceBlock.fullName
    case ctx: TypeDeclContext  => ctx.typeDecl.fullName
  }.get

  def enclosingTypeDeclFullName: Option[String] = {
    stack.collectFirst { case ctx: TypeDeclContext => ctx.typeDecl.fullName }
  }

  def rustParentFullName: String = {
    rustItemParentFullName(stack)
  }

  def parentIsMethod: Boolean = {
    ContextStack.parentIsMethod(stack)
  }

  def resolvedVariableReferences: Seq[(NewIdentifier, NewLocal | NewMethodParameterIn)] = {
    variableReferences.flatMap { case (identifier, stack) => lookup(identifier.name, stack).map((identifier, _)) }.toSeq
  }

}
