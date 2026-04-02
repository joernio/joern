package io.joern.abap2cpg.passes

import io.joern.abap2cpg.astcreation.AstHelpers
import io.joern.abap2cpg.astcreation.declarations.{AstForDeclarationsCreator, AstForParametersCreator}
import io.joern.abap2cpg.astcreation.expressions.AstForExpressionsCreator
import io.joern.abap2cpg.astcreation.statements.AstForStatementsCreator
import io.joern.abap2cpg.parser.AbapIntermediateAst
import io.joern.abap2cpg.parser.AbapIntermediateAst.*
import io.joern.x2cpg.{Ast, AstCreatorBase, ValidationMode}
import io.joern.x2cpg.datastructures.Stack.*
import io.joern.x2cpg.datastructures.VariableScopeManager
import io.shiftleft.codepropertygraph.generated.*
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.types.structure.NamespaceTraversal

/** Creates CPG from ABAP intermediate representation using Ast builder pattern
  */
class AstCreator(val program: ProgramRoot, filename: String)(implicit val withSchemaValidation: ValidationMode)
    extends AstCreatorBase[AbapNode, AstCreator](filename)
    with AstHelpers
    with AstForParametersCreator
    with AstForExpressionsCreator
    with AstForStatementsCreator
    with AstForDeclarationsCreator {

  val scope = new VariableScopeManager()

  // Required abstract methods from AstCreatorBase
  protected def code(node: AbapNode): String = codeFromExpr(node)

  protected def line(node: AbapNode): Option[Int] = node match {
    case n: MethodDef => n.span.start.map(_.row)
    case n: CallExpr => n.span.start.map(_.row)
    case n: AssignmentStmt => n.span.start.map(_.row)
    case n: OperatorCall => n.span.start.map(_.row)
    case n: DataDeclaration => n.span.start.map(_.row)
    case n: IdentifierExpr => n.span.start.map(_.row)
    case n: LiteralExpr => n.span.start.map(_.row)
    case n: FieldAccessExpr => n.span.start.map(_.row)
    case _ => None
  }

  protected def column(node: AbapNode): Option[Int] = node match {
    case n: MethodDef => n.span.start.map(_.col)
    case n: CallExpr => n.span.start.map(_.col)
    case n: AssignmentStmt => n.span.start.map(_.col)
    case n: OperatorCall => n.span.start.map(_.col)
    case n: DataDeclaration => n.span.start.map(_.col)
    case n: IdentifierExpr => n.span.start.map(_.col)
    case n: LiteralExpr => n.span.start.map(_.col)
    case n: FieldAccessExpr => n.span.start.map(_.col)
    case _ => None
  }

  protected def lineEnd(node: AbapNode): Option[Int] = node match {
    case n: MethodDef => n.span.end.map(_.row)
    case n: CallExpr => n.span.end.map(_.row)
    case n: AssignmentStmt => n.span.end.map(_.row)
    case n: OperatorCall => n.span.end.map(_.row)
    case n: DataDeclaration => n.span.end.map(_.row)
    case n: IdentifierExpr => n.span.end.map(_.row)
    case n: LiteralExpr => n.span.end.map(_.row)
    case n: FieldAccessExpr => n.span.end.map(_.row)
    case _ => None
  }

  protected def columnEnd(node: AbapNode): Option[Int] = node match {
    case n: MethodDef => n.span.end.map(_.col)
    case n: CallExpr => n.span.end.map(_.col)
    case n: AssignmentStmt => n.span.end.map(_.col)
    case n: OperatorCall => n.span.end.map(_.col)
    case n: DataDeclaration => n.span.end.map(_.col)
    case n: IdentifierExpr => n.span.end.map(_.col)
    case n: LiteralExpr => n.span.end.map(_.col)
    case n: FieldAccessExpr => n.span.end.map(_.col)
    case _ => None
  }

  /** Main entry point - creates the full CPG for a program */
  override def createAst(): DiffGraphBuilder = {
    val fileNode = NewFile()
      .name(program.fileName)
      .order(0)

    val namespaceBlock = NewNamespaceBlock()
      .name(NamespaceTraversal.globalNamespaceName)
      .fullName(s"$filename:<global>")
      .filename(filename)
      .order(1)

    // Create methods (either in classes or standalone)
    val methodAsts = if (program.classes.nonEmpty) {
      // Methods inside classes
      program.classes.zipWithIndex.flatMap { case (classDef, classIdx) =>
        val typeDecl = NewTypeDecl()
          .name(classDef.name)
          .fullName(classDef.name)
          .isExternal(false)
          .filename(program.fileName)
          .order(classIdx + 1)

        classDef.span.start.foreach { pos =>
          typeDecl.lineNumber(pos.row).columnNumber(pos.col)
        }

        val methodAsts = classDef.methods.zipWithIndex.map { case (method, methodIdx) =>
          astForMethod(method, Some(classDef.name), methodIdx + 1)
        }

        Ast(typeDecl).withChildren(methodAsts) :: Nil
      }
    } else {
      // Standalone methods
      program.methods.zipWithIndex.map { case (method, idx) =>
        astForMethod(method, None, idx + 1)
      }
    }

    val ast = Ast(fileNode)
      .withChild(Ast(namespaceBlock).withChildren(methodAsts))

    Ast.storeInDiffGraph(ast, diffGraph)
    scope.createVariableReferenceLinks(diffGraph, filename)
    diffGraph
  }
}
