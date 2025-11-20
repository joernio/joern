package io.joern.swiftsrc2cpg.astcreation

import io.joern.x2cpg.datastructures.VariableScopeManager
import io.joern.x2cpg.datastructures.VariableScopeManager.ScopeType
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
    val members = typeDeclToMemberMap.get(typeDeclFullName.stripSuffix("<extension>"))
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

}
