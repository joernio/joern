package io.joern.swiftsrc2cpg.passes

import io.joern.swiftsrc2cpg.astcreation.SwiftSrcGlobal
import io.shiftleft.codepropertygraph.generated.nodes.{NewMember, TypeDecl}
import io.shiftleft.codepropertygraph.generated.{Cpg, EdgeTypes}
import io.shiftleft.passes.ForkJoinParallelCpgPass
import io.shiftleft.semanticcpg.language.*

/** Pass that adds all members discovered from Swift extensions (as computed properties) to their respective type
  * declarations in the CPG.
  *
  * @param cpg
  *   the code property graph to update
  * @param extensionMembers
  *   mapping from TypeDecl fullNames to the members defined in extensions as computed properties
  */
class ExtensionMemberPass(cpg: Cpg, extensionMembers: Map[String, List[SwiftSrcGlobal.MemberInfo]])
    extends ForkJoinParallelCpgPass[(String, List[SwiftSrcGlobal.MemberInfo])](cpg) {

  override def generateParts(): Array[(String, List[SwiftSrcGlobal.MemberInfo])] = extensionMembers.traversal.toArray

  private def addMembers(builder: DiffGraphBuilder, decl: TypeDecl, members: List[SwiftSrcGlobal.MemberInfo]): Unit = {
    members.foreach { member =>
      val memberNode = NewMember()
        .name(member.name)
        .code(member.code)
        .typeFullName(member.typeFullName)
      builder.addNode(memberNode)
      builder.addEdge(decl, memberNode, EdgeTypes.AST)
    }
  }

  /** Process a single part of the work (a mapping entry) and update the diff graph with member information discovered
    * from extensions.
    *
    * @param builder
    *   the diff graph builder used to record node property changes and edges
    * @param part
    *   a tuple where the first element is the fullName of the `TypeDecl` to update and the second element is the list
    *   of members defined in extensions as computed properties
    */
  override def runOnPart(builder: DiffGraphBuilder, part: (String, List[SwiftSrcGlobal.MemberInfo])): Unit = {
    val (fullName, members) = part
    if (members.isEmpty) return

    def findTypeDecl(name: String): Option[TypeDecl] = {
      // If we cannot find the typeDecl by fullName, try looking it up by name (last part of fullName).
      // This is a fallback / the best effort approach for cases where the fullName is not accurate due to missing compiler support.
      cpg.typeDecl.fullNameExact(name).headOption.orElse {
        if (name.contains('.')) {
          val simpleName = name.split('.').last
          cpg.typeDecl.nameExact(simpleName).headOption
        } else None
      }
    }

    findTypeDecl(fullName).foreach { typeDecl =>
      addMembers(builder, typeDecl, members)
    }
  }

}
