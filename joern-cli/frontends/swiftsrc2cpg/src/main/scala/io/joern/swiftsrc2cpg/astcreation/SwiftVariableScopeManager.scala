package io.joern.swiftsrc2cpg.astcreation

import io.joern.x2cpg.Defines
import io.joern.x2cpg.datastructures.VariableScopeManager
import io.joern.x2cpg.datastructures.VariableScopeManager.*
import io.shiftleft.codepropertygraph.generated.nodes.NewNode

import scala.collection.mutable

class SwiftVariableScopeManager extends VariableScopeManager {

  override val ScopePathSeparator: String = "."

  private case class MemberElement(variableName: String, variableNode: NewNode, tpe: String)

  // mapping from TypDecl fullname to members to access members from extensions
  private val typeDeclToMemberMap = mutable.HashMap.empty[String, mutable.ArrayBuffer[MemberElement]]

  def typeDeclFullNameForMember(variableName: String): Option[String] = {
    typeDeclToMemberMap.collectFirst {
      case (typeDeclFullName, members) if members.exists(_.variableName == variableName) => typeDeclFullName
    }
  }

  def restoreMembersForExtension(typeDeclFullName: String): Unit = {
    val members = typeDeclToMemberMap.get(typeDeclFullName)
    members.foreach(memberList =>
      memberList.foreach(memberElement =>
        super.addVariable(
          memberElement.variableName,
          memberElement.variableNode,
          memberElement.tpe,
          ScopeType.TypeDeclScope
        )
      )
    )
  }

  override def addVariable(variableName: String, variableNode: NewNode, tpe: String, scopeType: ScopeType): Unit = {
    super.addVariable(variableName, variableNode, tpe, scopeType)
    if (scopeType == ScopeType.TypeDeclScope) {
      val typeDeclFullName = this.getEnclosingTypeDeclFullName
      if (typeDeclFullName.isDefined) {
        val members = typeDeclToMemberMap.getOrElse(typeDeclFullName.get, mutable.ArrayBuffer.empty)
        members.addOne(MemberElement(variableName, variableNode, tpe))
        typeDeclToMemberMap(typeDeclFullName.get) = members
      }
    }
  }

  private def getAllEnclosingFullNames(
    scopeHead: Option[ScopeElement],
    appendSignatureLast: Boolean = false
  ): Seq[String] = {
    scopeHead
      .collect {
        case methodScope: MethodScopeElement if appendSignatureLast =>
          val name = methodScope.methodName
          val nameWithSignature =
            methodScope.methodFullName.substring(methodScope.methodFullName.indexOf(s".$name:") + 1)
          nameWithSignature +: getAllEnclosingFullNames(methodScope.surroundingScope)
        case methodScope: MethodScopeElement =>
          methodScope.methodName +: getAllEnclosingFullNames(methodScope.surroundingScope)
        case typeDeclScope: TypeDeclScopeElement =>
          typeDeclScope.name +: getAllEnclosingFullNames(typeDeclScope.surroundingScope)
        case other =>
          getAllEnclosingFullNames(other.surroundingScope)
      }
      .getOrElse(Seq.empty)
  }

  /** Compute the scope path for the current stack.
    *
    * We override the base implementation to provide Swift-specific behavior: We need to ensure the produced scope path
    * is stable for Swift constructs (including extensions) and does not contain redundant repeated segments.
    *
    * What `collapsed` does:
    *   - `getAllEnclosingFullNames(stack)` yields names from the innermost scope to the outermost.
    *   - We `reverse` that sequence so we iterate from outermost to innermost.
    *   - The `foldLeft` builds a sequence while skipping a name if it is the same as the last appended one. This
    *     removes adjacent duplicates (caused by extensions with the same name) while preserving the overall ordering.
    *
    * The final path is the collapsed segments joined by `ScopePathSeparator`.
    */
  override def computeScopePath: String = {
    val collapsed = getAllEnclosingFullNames(stack).reverse.foldLeft(Seq.empty[String]) {
      case (acc, name) if acc.lastOption.contains(name) => acc
      case (acc, name)                                  => acc :+ name
    }
    collapsed.mkString(ScopePathSeparator)
  }

  def computeScopePathWithSignatures: String = {
    val collapsed = getAllEnclosingFullNames(stack, appendSignatureLast = true).reverse.foldLeft(Seq.empty[String]) {
      case (acc, name) if acc.lastOption.contains(name) => acc
      case (acc, name)                                  => acc :+ name
    }
    collapsed.mkString(ScopePathSeparator)
  }

  def isInStaticMethodScope: Boolean = {
    def isClosureScope(methodScope: MethodScopeElement): Boolean = {
      methodScope.methodName.startsWith(Defines.ClosurePrefix)
    }

    def nextNonClosureMethodScope(scopeHead: Option[ScopeElement]): Option[MethodScopeElement] = {
      scopeHead.flatMap {
        case ms: MethodScopeElement if !isClosureScope(ms) => Some(ms)
        case ms: MethodScopeElement                        => nextNonClosureMethodScope(ms.surroundingScope)
        case bs: BlockScopeElement                         => nextNonClosureMethodScope(bs.surroundingScope)
        case _: TypeDeclScopeElement                       => None
      }
    }

    nextNonClosureMethodScope(stack).exists(_.isStatic)
  }

}
