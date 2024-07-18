package io.shiftleft.semanticcpg.language.modulevariable.nodemethods

import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.{CfgNode, FieldIdentifier, Identifier, Import, Member, TypeDecl}
import io.shiftleft.semanticcpg.language.modulevariable.OpNodes
import io.shiftleft.semanticcpg.language.operatorextension.OpNodes as OpExtNodes
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.language.importresolver.{ResolvedMember, ResolvedTypeDecl}
import io.shiftleft.codepropertygraph.generated.help.Doc

class ModuleVariableMethods(node: OpNodes.ModuleVariable) extends AnyVal {

  /** References of this module variable across the codebase, as either identifiers or field identifiers */
  def references: Iterator[Identifier | FieldIdentifier] = node.start.references

  /** The module members being referenced in the respective module type declaration */
  def referencingMembers: Iterator[Member] = {
    Cpg(node.graph).typeDecl.fullNameExact(node.method.fullName.toSeq*).member.nameExact(node.name)
  }

  /** Returns the assignments where the module variable is the target (LHS) */
  def definitions: Iterator[OpExtNodes.Assignment] = node.start.definitions

}
