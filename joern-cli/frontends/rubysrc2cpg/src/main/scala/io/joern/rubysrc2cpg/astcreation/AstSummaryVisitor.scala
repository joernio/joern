package io.joern.rubysrc2cpg.astcreation

import better.files.File
import io.joern.rubysrc2cpg.astcreation.RubyIntermediateAst.StatementList
import io.joern.rubysrc2cpg.datastructures.{RubyField, RubyMethod, RubyProgramSummary, RubyStubbedType, RubyType}
import io.joern.rubysrc2cpg.parser.RubyNodeCreator
import io.joern.rubysrc2cpg.passes.Defines
import io.joern.x2cpg.layers.Base
import io.joern.x2cpg.passes.base.{AstLinkerPass, FileCreationPass}
import io.joern.x2cpg.{Ast, ValidationMode}
import io.shiftleft.codepropertygraph.cpgloading.CpgLoader
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.{Local, Member, Method, TypeDecl}
import io.shiftleft.semanticcpg.language.*
import overflowdb.{BatchedUpdate, Config}

import java.io.File as JavaFile
import java.util.regex.Matcher
import scala.collection.mutable
import scala.util.Using

trait AstSummaryVisitor(implicit withSchemaValidation: ValidationMode) { this: AstCreator =>

  def summarize(asExternal: Boolean = false): RubyProgramSummary = {
    this.parseLevel = AstParseLevel.SIGNATURES
    Using.resource(Cpg.empty) { cpg =>
      // Build and store compilation unit AST
      val rootNode = new RubyNodeCreator().visit(programCtx).asInstanceOf[StatementList]
      val ast      = astForRubyFile(rootNode)
      Ast.storeInDiffGraph(ast, diffGraph)
      BatchedUpdate.applyDiff(cpg.graph, diffGraph)
      CpgLoader.createIndexes(cpg)
      // Link basic AST elements
      AstLinkerPass(cpg).createAndApply()
      // Summarize findings
      summarize(cpg, asExternal)
    }
  }

  def withSummary(newSummary: RubyProgramSummary): AstCreator = {
    AstCreator(fileName, programCtx, projectRoot, newSummary)
  }

  private def summarize(cpg: Cpg, asExternal: Boolean): RubyProgramSummary = {
    val gemName =
      if relativeFileName.nonEmpty && relativeFileName.contains(JavaFile.separator) then
        relativeFileName.substring(0, relativeFileName.indexOf(JavaFile.separator))
      else Option(relativeFileName).getOrElse("")

    def toMethod(m: Method): RubyMethod = {
      val definingTypeDeclFullName =
        if asExternal then
          Option(m.definingTypeDecl.map(buildFullName)).getOrElse(m.definingTypeDecl.fullName.headOption)
        else m.definingTypeDecl.fullName.headOption

      RubyMethod(
        m.name,
        m.parameter.map(x => x.name -> x.typeFullName).l,
        m.methodReturn.typeFullName,
        definingTypeDeclFullName
      )
    }

    def toField(f: Member): RubyField = {
      RubyField(f.name, f.typeFullName)
    }

    def toModuleVariable(v: Local): RubyField = {
      RubyField(v.name, v.typeFullName)
    }

    def toType(m: TypeDecl): RubyType = {
      if asExternal then RubyStubbedType(buildFullName(m), m.method.map(toMethod).l, m.member.map(toField).l)
      else RubyType(m.fullName, m.method.map(toMethod).l, m.member.map(toField).l)
    }

    def buildFullName(m: TypeDecl): String = if asExternal then
      m.start
        .repeat(_.astParent)(_.until(_.isMethod.isModule))
        .cast[Method]
        .fullName
        .map(mfn => s"$gemName${m.fullName.stripPrefix(mfn)}")
        .headOption
        .getOrElse(m.fullName)
    else m.fullName

    def handleNestedTypes(t: TypeDecl, parentScope: String): Seq[(String, Set[RubyType])] = {
      val typeFullName     = s"$parentScope.${t.name}"
      val childrenTypes    = t.astChildren.collectAll[TypeDecl].l
      val typesOnThisLevel = childrenTypes.flatMap(handleNestedTypes(_, typeFullName))
      Seq(typeFullName -> childrenTypes.whereNot(_.methodBinding).map(toType).toSet) ++ typesOnThisLevel
    }

    val mappings =
      cpg.namespaceBlock.flatMap { namespace =>
        val path = namespace.filename
          .replaceAll(Matcher.quoteReplacement(JavaFile.separator), "/") // handle Windows paths
          .stripSuffix(".rb")
        // Map module functions/variables
        val namespaceFullName =
          if asExternal then gemName
          else namespace.fullName

        val moduleEntry = (path, namespaceFullName) -> namespace.method.map { module =>
          val moduleFullName =
            if asExternal then gemName
            else module.fullName

          val moduleTypeMap =
            RubyType(
              moduleFullName,
              module.astChildren.collectAll[Method].map(toMethod).l,
              module.local.map(toModuleVariable).l
            )
          moduleTypeMap
        }.toSet
        // Map module types
        val typeEntries = namespace.method.collectFirst {
          case m: Method if m.name == Defines.Main =>
            val childrenTypes = m.astChildren.collectAll[TypeDecl].l
            val fullName =
              if childrenTypes.nonEmpty && asExternal then buildFullName(childrenTypes.head) else s"${m.fullName}"
            val nestedTypes = childrenTypes.flatMap(handleNestedTypes(_, fullName))
            (path, fullName) -> (childrenTypes.whereNot(_.methodBinding).map(toType).toSet ++ nestedTypes.flatMap(_._2))
        }.toSeq

        moduleEntry +: typeEntries
      }.toList

    val namespaceMappings: mutable.Map[String, mutable.Set[RubyType]] =
      mutable.Map.from(mappings.map { case (_, ns) -> entry => ns -> mutable.Set.from(entry) })
    val pathMappings: mutable.Map[String, mutable.Set[RubyType]] =
      mutable.Map.from(mappings.map { case (path, _) -> entry => path -> mutable.Set.from(entry) })

    RubyProgramSummary(namespaceMappings, pathMappings)
  }

}
