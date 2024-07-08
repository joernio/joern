package io.joern.rubysrc2cpg.passes

import io.joern.x2cpg.Defines as XDefines
import io.joern.x2cpg.passes.frontend.*
import io.joern.x2cpg.passes.frontend.XTypeRecovery.AllNodeTypesFromNodeExt
import io.shiftleft.codepropertygraph.generated.{Cpg, Operators, PropertyNames}
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.operatorextension.OpNodes.FieldAccess
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.codepropertygraph.generated.DiffGraphBuilder

class RubyTypeRecoveryPassGenerator(cpg: Cpg, config: XTypeRecoveryConfig = XTypeRecoveryConfig())
    extends XTypeRecoveryPassGenerator[File](cpg, config) {
  override protected def generateRecoveryPass(state: XTypeRecoveryState, iteration: Int): XTypeRecovery[File] =
    new RubyTypeRecovery(cpg, state, iteration)
}

private class RubyTypeRecovery(cpg: Cpg, state: XTypeRecoveryState, iteration: Int)
    extends XTypeRecovery[File](cpg, state, iteration) {

  override def compilationUnits: Iterator[File] = cpg.file.iterator

  override def generateRecoveryForCompilationUnitTask(
    unit: File,
    builder: DiffGraphBuilder
  ): RecoverForXCompilationUnit[File] = {
    new RecoverForRubyFile(cpg, unit, builder, state)
  }
}

private class RecoverForRubyFile(cpg: Cpg, cu: File, builder: DiffGraphBuilder, state: XTypeRecoveryState)
    extends RecoverForXCompilationUnit[File](cpg, cu, builder, state) {

  /** A heuristic method to determine if a call is a constructor or not.
    */
  override protected def isConstructor(c: Call): Boolean = {
    isConstructor(c.name) && c.code.charAt(0).isUpper
  }

  /** A heuristic method to determine if a call name is a constructor or not.
    */
  override protected def isConstructor(name: String): Boolean =
    !name.isBlank && (name == "new" || name == Defines.Initialize)

  override protected def hasTypes(node: AstNode): Boolean = node match {
    case x: Call if !x.methodFullName.startsWith("<operator>") =>
      x.getKnownTypes.nonEmpty
    case x: Call if x.methodFullName.startsWith("<operator>") =>
      x.typeFullName != "<empty>" && super.hasTypes(node)
    case x =>
      x.getKnownTypes.nonEmpty
  }

  override def prepopulateSymbolTableEntry(x: AstNode): Unit = x match {
    case x @ (_: Identifier | _: Local | _: MethodParameterIn) => symbolTable.append(x, x.getKnownTypes)
    case call: Call =>
      val tnfs =
        if call.methodFullName == XDefines.DynamicCallUnknownFullName || call.methodFullName.startsWith("<operator>")
        then (call.dynamicTypeHintFullName ++ call.possibleTypes).distinct
        else (call.methodFullName +: (call.dynamicTypeHintFullName ++ call.possibleTypes)).distinct

      symbolTable.append(call, tnfs.toSet)
    case _ =>
  }

  override def visitImport(i: Import): Unit = for {
    resolvedImport <- i.call.tag
    alias          <- i.importedAs
  } {
    import io.shiftleft.semanticcpg.language.importresolver.*
    EvaluatedImport.tagToEvaluatedImport(resolvedImport).foreach {
      case ResolvedTypeDecl(fullName, _) =>
        symbolTable.append(LocalVar(fullName.split("\\.").lastOption.getOrElse(alias)), fullName)
      case _ => super.visitImport(i)
    }
  }

  override def visitIdentifierAssignedToConstructor(i: Identifier, c: Call): Set[String] = {
    associateTypes(i, Set(i.typeFullName))
  }

  override def storeCallTypeInfo(c: Call, types: Seq[String]): Unit =
    if (types.nonEmpty) {

      // Only necessary if we have more than 1 type and want to try and resolve to a single type
      val finalTypes = if (types.size > 1 && c.receiver.nonEmpty) {
        c.receiver.l.isCall.headOption match {
          case Some(recCall) =>
            if (recCall.methodFullName == Operators.fieldAccess) {
              val fieldAccessCall = recCall.asInstanceOf[FieldAccess]
              val fieldAccessName = getFieldName(fieldAccessCall) // Returns Module1.foo for ex when it can be resolved
              val fieldAccessParents = getFieldParents(fieldAccessCall)
              // Some FieldAccess return unknown (ex regex: 'x' =~ /y/) so we return types since we cannot resolve further
              if (fieldAccessName == "<unknown>")
                types
              else
                fieldAccessParents
                  .filter(_.endsWith(fieldAccessName.stripSuffix(s".${c.name}")))
                  .map(x => s"$x:${c.name}")
            } else {
              types
            }
          case None =>
            types
        }
      } else {
        types
      }

      builder.setNodeProperty(
        c,
        PropertyNames.DYNAMIC_TYPE_HINT_FULL_NAME,
        (c.dynamicTypeHintFullName ++ finalTypes).distinct
      )
    }
}
