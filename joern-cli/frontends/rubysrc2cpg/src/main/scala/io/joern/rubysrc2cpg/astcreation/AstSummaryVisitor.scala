package io.joern.rubysrc2cpg.astcreation

import io.joern.rubysrc2cpg.astcreation.RubyIntermediateAst.{
  ClassDeclaration,
  FieldsDeclaration,
  MandatoryParameter,
  MethodDeclaration,
  ModuleDeclaration,
  OptionalParameter,
  RubyNode,
  SimpleIdentifier,
  StatementList,
  TypeDeclaration
}
import io.joern.rubysrc2cpg.datastructures.{
  NamespaceScope,
  RubyField,
  RubyMethod,
  RubyProgramSummary,
  RubyType,
  TypeScope
}
import io.joern.rubysrc2cpg.passes.Defines
import io.joern.rubysrc2cpg.parser.RubyNodeCreator
import io.joern.x2cpg.datastructures.ProgramSummary
import io.joern.x2cpg.{ValidationMode, Defines as XDefines}
import io.shiftleft.codepropertygraph.generated.nodes.{NewMember, NewMethod, NewMethodParameterIn}
import io.shiftleft.semanticcpg.language.types.structure.NamespaceTraversal

trait AstSummaryVisitor(implicit withSchemaValidation: ValidationMode) { this: AstCreator =>

  private def baseNamespace: String = s"$relativeFileName:${NamespaceTraversal.globalNamespaceName}"

  def summarize(): RubyProgramSummary = {
    val rootNode = new RubyNodeCreator().visit(this.programCtx).asInstanceOf[StatementList]
    val fullName = baseNamespace
    scope.pushNewScope(NamespaceScope(fullName))

    val newMap = scope.newProgramScope
      .map { moduleScope =>
        scope.pushNewScope(moduleScope)
        val m = rootNode.statements
          .map(visitStatement)
          .reduceOption((a, b) => ProgramSummary.combine(a, b))
          .getOrElse(Map.empty)
        scope.popScope()
        m
      }
      .getOrElse(Map.empty)

    scope.popScope()
    RubyProgramSummary(newMap)
  }

  def withSummary(newSummary: RubyProgramSummary): AstCreator = {
    AstCreator(fileName, programCtx, projectRoot, newSummary)
  }

  private def visitStatement(stmt: RubyNode): Map[String, Set[RubyType]] = stmt match {
    case node: TypeDeclaration => visitTypeDeclaration(node)
    case _                     => Map.empty
  }

  private def visitTypeDeclaration(classDecl: TypeDeclaration): Map[String, Set[RubyType]] = {
    classDecl.name match {
      case name: SimpleIdentifier =>
        val fullName = computeClassFullName(name.text)
        Map(
          scope.surroundingScopeFullName
            .getOrElse(baseNamespace) -> Set(visitTypeLikeDeclaration(fullName, classDecl.body))
        )
      case _ => Map.empty
    }
  }

  private def visitTypeLikeDeclaration(fullName: String, body: RubyNode): RubyType = {
    scope.pushNewScope(TypeScope(fullName))
    val classBody = body.asInstanceOf[StatementList]
    val bodyMap =
      classBody.statements.flatMap {
        case MethodDeclaration(methodName, parameters, _) =>
          RubyMethod(methodName, visitParameters(parameters), XDefines.Any) :: Nil
        case node: FieldsDeclaration =>
          astsForFieldDeclarations(node).flatMap(_.nodes).collect {
            case x: NewMember => RubyField(x.name, x.typeFullName)
            case x: NewMethod => RubyMethod(x.name, List.empty, XDefines.Any) // These are getters/setters
          }
        case _ => Seq.empty
      }
    scope.popScope()
    RubyType(fullName, bodyMap.collect { case x: RubyMethod => x }, bodyMap.collect { case x: RubyField => x })
  }

  private def visitParameters(parameters: List[RubyNode]): List[(String, String)] = {
    parameters.map(astForParameter(_, -1)).flatMap(_.root).collect { case x: NewMethodParameterIn =>
      (x.name, x.typeFullName)
    }
  }

}
