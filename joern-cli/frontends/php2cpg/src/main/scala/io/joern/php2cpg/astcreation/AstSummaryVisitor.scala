package io.joern.php2cpg.astcreation

import flatgraph.DiffGraphApplier
import io.joern.php2cpg.datastructures.*
import io.joern.x2cpg.datastructures.AstParseLevel
import io.joern.x2cpg.datastructures.AstParseLevel.{FULL_AST, SIGNATURES}
import io.joern.x2cpg.passes.base.AstLinkerPass
import io.joern.x2cpg.{Ast, ValidationMode}
import io.shiftleft.codepropertygraph.generated.nodes.Namespace
import io.shiftleft.codepropertygraph.generated.{Cpg, nodes}
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.language.types.structure.NamespaceTraversal

import scala.collection.mutable
import scala.util.Using

trait AstSummaryVisitor(implicit withSchemaValidation: ValidationMode) { this: AstCreator =>

  def summarize: PhpProgramSummary = this.parseLevel match {
    case FULL_AST =>
      AstCreator(relativeFileName, fileName, phpAst, disableFileContent, programSummary, SIGNATURES).summarize
    case SIGNATURES =>
      Using.resource(Cpg.empty) { cpg =>
        // Build and store compilation unit AST
        val ast = astForPhpFile(phpAst)
        Ast.storeInDiffGraph(ast, diffGraph)
        DiffGraphApplier.applyDiff(cpg.graph, diffGraph)

        // Link basic AST elements
        AstLinkerPass(cpg).createAndApply()
        // Summarize findings
        summarize(cpg)
      }
  }

  def withSummary(newSummary: PhpProgramSummary): AstCreator = {
    AstCreator(relativeFileName, fileName, phpAst, disableFileContent, newSummary, parseLevel)
  }

  private def summarize(cpg: Cpg): PhpProgramSummary = {

    def toMethod(m: nodes.Method): PhpMethod = {
      val definingTypeDeclFullName = m.definingTypeDecl.fullName.headOption

      PhpMethod(
        m.name,
        m.parameter.map(x => x.name -> x.typeFullName).l,
        m.methodReturn.typeFullName,
        definingTypeDeclFullName
      )
    }

    def toField(f: nodes.Member): PhpField = {
      PhpField(f.name, f.typeFullName)
    }

    def toType(m: nodes.TypeDecl): PhpType = PhpType(m.fullName, m.method.map(toMethod).l, m.member.map(toField).l)

    val namespaceToTypeMap = cpg.namespaceBlock.map { n =>
      val phpTypes = n.astChildren.flatMap {
        // We have TypeDecl(<global>)->Method(<global>)->/.../
        case x: nodes.TypeDecl if x.name == NamespaceTraversal.globalNamespaceName =>
          val classDecls = x.method.isModule.block.astChildren.collect { case classDecl: nodes.TypeDecl =>
            toType(classDecl)
          }.toList
          val topLevelMethods = x.method.isModule.block.astChildren.collect { case functionDecl: nodes.Method =>
            toMethod(functionDecl)
          }.toList
          val topLevelTypeDecl = PhpType(n.fullName, topLevelMethods, Nil)
          topLevelTypeDecl :: classDecls
        case x: nodes.Method => PhpType(n.fullName, toMethod(x) :: Nil, Nil) :: Nil
        case _               => Nil
      }.toSetMutable
      n.fullName -> phpTypes
    }.toSeq
    PhpProgramSummary(mutable.Map(namespaceToTypeMap*))
  }

}
