package io.joern.swiftsrc2cpg.astcreation

import io.joern.x2cpg.datastructures.VariableScopeManager
import io.joern.x2cpg.datastructures.VariableScopeManager.ScopeType
import io.shiftleft.codepropertygraph.generated.nodes.NewNode

import scala.collection.mutable

class SwiftVariableScopeManager extends VariableScopeManager {

  override val ScopePathSeparator: String = "."

  // to access members from extensions
  private val typeDeclToMemberMap = mutable.HashMap.empty[String, mutable.HashMap[String, String]]

  def typeDeclHasMember(typeDeclFullName: String, memberName: String): Boolean = {
    typeDeclToMemberMap.get(typeDeclFullName.stripSuffix("<extension>")).exists(_.contains(memberName))
  }

  def memberTypeFromTypeDeclExtension(typeDeclFullName: String, memberName: String): Option[String] = {
    typeDeclToMemberMap
      .get(typeDeclFullName.stripSuffix("<extension>"))
      .collect { case memberMap if memberMap.contains(memberName) => memberMap(memberName) }
  }

  override def addVariable(variableName: String, variableNode: NewNode, tpe: String, scopeType: ScopeType): Unit = {
    super.addVariable(variableName, variableNode, tpe, scopeType)
    if (scopeType == ScopeType.TypeDeclScope) {
      val typeDeclFullName = this.getEnclosingTypeDeclFullName
      if (typeDeclFullName.isDefined) {
        val memberMap = typeDeclToMemberMap.getOrElse(typeDeclFullName.get, mutable.HashMap.empty)
        memberMap(variableName) = tpe
        typeDeclToMemberMap(typeDeclFullName.get) = memberMap
      }
    }
  }

}
