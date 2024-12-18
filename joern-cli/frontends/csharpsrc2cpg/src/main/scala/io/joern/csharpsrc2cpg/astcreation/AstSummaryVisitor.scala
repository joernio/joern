package io.joern.csharpsrc2cpg.astcreation

import flatgraph.DiffGraphApplier.applyDiff
import io.joern.csharpsrc2cpg.Constants
import io.joern.csharpsrc2cpg.datastructures.{
  CSharpField,
  CSharpMethod,
  CSharpProgramSummary,
  CSharpType,
  NamespaceToTypeMap
}

import io.joern.csharpsrc2cpg.parser.ParserKeys
import io.joern.x2cpg.{Ast, ValidationMode}
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.codepropertygraph.generated.{Cpg, EdgeTypes}
import io.shiftleft.semanticcpg.language.*

import scala.collection.mutable
import scala.util.Using

/** Allows the AST creator to run at a signature-only level and query the resulting CPG to build up a look-ahead cache.
  */
trait AstSummaryVisitor(implicit withSchemaValidation: ValidationMode) { this: AstCreator =>

  /** Does a high-level parse of the compilation unit to obtain type and method signature information.
    */
  def summarize(): CSharpProgramSummary = {
    this.parseLevel = AstParseLevel.SIGNATURES
    val fileNode        = NewFile().name(relativeFileName)
    val compilationUnit = createDotNetNodeInfo(parserResult.json(ParserKeys.AstRoot))
    Using.resource(Cpg.empty) { cpg =>
      // Build and store compilation unit AST
      val ast = Ast(fileNode).withChildren(astForCompilationUnit(compilationUnit))
      Ast.storeInDiffGraph(ast, diffGraph)
      applyDiff(cpg.graph, diffGraph)

      // Simulate AST Linker for global namespace
      val globalNode      = NewNamespaceBlock().fullName(Constants.Global).name(Constants.Global)
      val globalDiffGraph = Cpg.newDiffGraphBuilder
      cpg.typeDecl
        .where(_.astParentFullNameExact(Constants.Global))
        .foreach(globalDiffGraph.addEdge(globalNode, _, EdgeTypes.AST))
      applyDiff(cpg.graph, globalDiffGraph)

      // Summarize findings
      summarize(cpg)
    }
  }

  /** Creates a deep copy of the AST creator with the new summary.
    */
  def withSummary(newSummary: CSharpProgramSummary): AstCreator = {
    AstCreator(relativeFileName, parserResult, newSummary)
  }

  private def summarize(cpg: Cpg): CSharpProgramSummary = {

    def imports = cpg.imports.importedEntity.toSet

    def globalImports = cpg.imports.filter(_.code.startsWith("global")).importedEntity.toSet

    def toMethod(m: Method): CSharpMethod = {
      CSharpMethod(
        m.name,
        m.methodReturn.typeFullName,
        m.parameter.map(x => x.name -> x.typeFullName).l,
        m.isStatic.nonEmpty
      )
    }

    def toField(f: Member): CSharpField = {
      CSharpField(f.name, f.typeFullName)
    }

    val mapping = mutable.Map
      .from(cpg.namespaceBlock.map { namespace =>
        namespace.fullName -> mutable.Set.from(namespace.typeDecl.map { typ =>
          CSharpType(typ.fullName, typ.method.map(toMethod).l, typ.member.map(toField).l)
        })
      })
      .asInstanceOf[NamespaceToTypeMap]
    CSharpProgramSummary(mapping, imports, globalImports)
  }

}
