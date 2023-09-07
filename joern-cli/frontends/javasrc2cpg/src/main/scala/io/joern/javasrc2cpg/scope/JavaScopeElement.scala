package io.joern.javasrc2cpg.scope

import io.joern.javasrc2cpg.scope.Scope.*
import io.joern.javasrc2cpg.scope.JavaScopeElement.*
import io.shiftleft.codepropertygraph.generated.nodes.{
  NewImport,
  NewMethod,
  NewNamespaceBlock,
  NewTypeDecl,
  NewTypeParameter
}

import scala.collection.mutable
import io.joern.javasrc2cpg.passes.ExpectedType
import io.joern.x2cpg.Ast
import io.shiftleft.codepropertygraph.generated.nodes.AstNodeNew

trait JavaScopeElement {
  private val variables                        = mutable.Map[String, ScopeVariable]()
  private val types                            = mutable.Map[String, String]()
  private var wildcardImports: WildcardImports = NoWildcard

  def addVariableToScope(variable: ScopeVariable): Unit = {
    variables.put(variable.name, variable)
  }

  def lookupVariable(name: String): Option[ScopeVariable] = {
    variables.get(name)
  }

  def addTypeToScope(name: String, typeFullName: String): Unit = {
    types.put(name, typeFullName)
  }

  def lookupType(name: String, includeWildcards: Boolean): Option[String] = {
    types.get(name) match {
      case None if includeWildcards => getNameWithWildcardPrefix(name)
      case result                   => result
    }
  }

  def getNameWithWildcardPrefix(name: String): Option[String] = {
    wildcardImports match {
      case SingleWildcard(prefix) => Some(s"$prefix.$name")

      case _ => None
    }
  }

  def addWildcardImport(prefix: String): Unit = {
    wildcardImports match {
      case NoWildcard => wildcardImports = SingleWildcard(prefix)

      case SingleWildcard(_) => wildcardImports = MultipleWildcards

      case MultipleWildcards => // Already MultipleWildcards, so change nothing
    }
  }

  // TODO: Refactor and remove this
  def getVariables(): List[ScopeVariable] = variables.values.toList
}

private object JavaScopeElement {
  sealed trait WildcardImports
  case object NoWildcard                    extends WildcardImports
  case class SingleWildcard(prefix: String) extends WildcardImports
  case object MultipleWildcards             extends WildcardImports
}

class NamespaceScope(val namespace: NewNamespaceBlock) extends JavaScopeElement with TypeDeclContainer

class BlockScope extends JavaScopeElement

class MethodScope(val method: NewMethod, val returnType: ExpectedType) extends JavaScopeElement

class TypeDeclScope(val typeDecl: NewTypeDecl) extends JavaScopeElement with TypeDeclContainer {
  private val memberInitializers = mutable.ListBuffer[Ast]()

  // TODO: Refactor and remove this.
  def addMemberInitializers(initializers: Seq[Ast]): Unit = {
    memberInitializers.appendAll(initializers)
  }

  // TODO: Refactor and remove this.
  def getMemberInitializers(): List[Ast] = {
    memberInitializers.toList.flatMap { ast =>
      ast.root match {
        case Some(root: AstNodeNew) =>
          Some(ast.subTreeCopy(root))

        case _ => None
      }
    }
  }
}
