package io.joern.rubysrc2cpg.astcreation

import io.joern.rubysrc2cpg.astcreation.RubyIntermediateAst.StatementList
import io.joern.rubysrc2cpg.datastructures.{RubyField, RubyMethod, RubyProgramSummary, RubyStubbedType, RubyType}
import io.joern.rubysrc2cpg.parser.RubyNodeCreator
import io.joern.rubysrc2cpg.passes.Defines
import io.joern.x2cpg.passes.base.AstLinkerPass
import io.joern.x2cpg.{Ast, ValidationMode}
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
    Using.resource(Cpg.withConfig(Config.withoutOverflow())) { cpg =>
      // Build and store compilation unit AST
      val rootNode = new RubyNodeCreator().visit(programCtx).asInstanceOf[StatementList]
      val ast      = astForRubyFile(rootNode)
      Ast.storeInDiffGraph(ast, diffGraph)
      BatchedUpdate.applyDiff(cpg.graph, diffGraph)

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
    def toMethod(m: Method): RubyMethod = {
      RubyMethod(
        m.name,
        m.parameter.map(x => x.name -> x.typeFullName).l,
        m.methodReturn.typeFullName,
        m.definingTypeDecl.fullName.headOption
      )
    }

    def toField(f: Member): RubyField = {
      RubyField(f.name, f.typeFullName)
    }

    def toModuleVariable(v: Local): RubyField = {
      RubyField(v.name, v.typeFullName)
    }

    def toType(m: TypeDecl): RubyType = {
      if asExternal then RubyStubbedType(m.fullName, m.method.map(toMethod).l, m.member.map(toField).l)
      else RubyType(m.fullName, m.method.map(toMethod).l, m.member.map(toField).l)
    }

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
        val moduleEntry = (path, namespace.fullName) -> namespace.method.map { module =>
          val moduleTypeMap =
            RubyType(
              module.fullName,
              module.block.astChildren.collectAll[Method].map(toMethod).l,
              module.local.map(toModuleVariable).l
            )
          moduleTypeMap
        }.toSet
        // Map module types
        val typeEntries = namespace.method.collectFirst {
          case m: Method if m.name == Defines.Program =>
            val childrenTypes = m.block.astChildren.collectAll[TypeDecl].l
            val fullName      = s"${namespace.fullName}:${m.name}"
            val nestedTypes   = childrenTypes.flatMap(handleNestedTypes(_, fullName))
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
