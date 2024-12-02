package io.joern.javasrc2cpg.astcreation.expressions

import com.github.javaparser.ast.Node
import com.github.javaparser.ast.expr.{
  Expression,
  InstanceOfExpr,
  NameExpr,
  PatternExpr,
  RecordPatternExpr,
  TypePatternExpr
}
import io.joern.javasrc2cpg.astcreation.{AstCreator, ExpectedType}
import io.joern.javasrc2cpg.jartypereader.model.Model.TypeConstants
import io.joern.javasrc2cpg.scope.Scope.NewVariableNode
import io.joern.x2cpg.Ast
import io.joern.x2cpg.utils.AstPropertiesUtil.*
import io.shiftleft.codepropertygraph.generated.nodes.{AstNodeNew, NewIdentifier}
import io.joern.x2cpg.utils.NodeBuilders.*
import io.shiftleft.codepropertygraph.generated.Operators

import scala.jdk.CollectionConverters.*

trait AstForPatternExpressionsCreator { this: AstCreator =>

  private[astcreation] def astIdentifierAndRefsForPatternLhs(
    rootNode: Node,
    patternInitAst: Ast
  ): (Ast, NewIdentifier, Option[NewVariableNode]) = {
    patternInitAst.nodes.toList match {
      case (identifier: NewIdentifier) :: Nil =>
        (patternInitAst, identifier, scope.lookupVariable(identifier.name).variableNode)

      case _ =>
        val tmpName       = tempNameProvider.next
        val tmpType       = patternInitAst.rootType.getOrElse(TypeConstants.Object)
        val tmpLocal      = localNode(rootNode, tmpName, tmpName, tmpType)
        val tmpIdentifier = identifierNode(rootNode, tmpName, tmpName, tmpType)

        val tmpAssignmentNode =
          newOperatorCallNode(
            Operators.assignment,
            s"$tmpName = ${patternInitAst.rootCodeOrEmpty}",
            Option(tmpType),
            line(rootNode),
            column(rootNode)
          )

        // Don't need to add the local to the block scope since the only identifiers referencing it are created here
        // (so a lookup for the local will never be done)
        scope.enclosingMethod.foreach(_.addTemporaryLocal(tmpLocal))

        (
          callAst(tmpAssignmentNode, Ast(tmpIdentifier) :: patternInitAst :: Nil).withRefEdge(tmpIdentifier, tmpLocal),
          tmpIdentifier,
          Option(tmpLocal)
        )
    }
  }

  private[astcreation] def astForInstanceOfWithPattern(
    instanceOfLhsExpr: Expression,
    patternLhsInitAst: Ast,
    pattern: PatternExpr
  ): Ast = {
    val (lhsAst, lhsIdentifier, lhsRefsTo) = astIdentifierAndRefsForPatternLhs(instanceOfLhsExpr, patternLhsInitAst)

    val patternTypeFullName = {
      tryWithSafeStackOverflow(pattern.getType).toOption
        .map(typ =>
          scope
            .lookupScopeType(typ.asString())
            .map(_.typeFullName)
            .orElse(typeInfoCalc.fullName(typ))
            .getOrElse(defaultTypeFallback(typ))
        )
    }.getOrElse(defaultTypeFallback())

    val patternTypeRef = typeRefNode(pattern.getType, code(pattern.getType), patternTypeFullName)

    val typePatterns = getTypePatterns(pattern)

    typePatterns.foreach { typePatternExpr =>
      val variableName = typePatternExpr.getNameAsString
      val variableType = {
        tryWithSafeStackOverflow(typePatternExpr.getType).toOption
          .map(typ =>
            scope
              .lookupScopeType(typ.asString())
              .map(_.typeFullName)
              .orElse(typeInfoCalc.fullName(typ))
              .getOrElse(defaultTypeFallback(typ))
          )
          .getOrElse(defaultTypeFallback())
      }
      val variableTypeCode = tryWithSafeStackOverflow(code(typePatternExpr.getType)).getOrElse(variableType)

      val patternLocal      = localNode(typePatternExpr, variableName, code(typePatternExpr), variableType)
      val patternIdentifier = identifierNode(typePatternExpr, variableName, variableName, variableType)
      // TODO Handle record pattern initializers
      val patternInitializerCastType = typeRefNode(typePatternExpr, code(typePatternExpr.getType), variableType)
      val patternInitializerCastRhs  = lhsIdentifier.copy
      val patternInitializerCast = newOperatorCallNode(
        Operators.cast,
        s"($variableTypeCode) ${lhsIdentifier.code}",
        Option(variableType),
        line(typePatternExpr),
        column(typePatternExpr)
      )

      val initializerCastAst =
        callAst(patternInitializerCast, Ast(patternInitializerCastType) :: Ast(patternInitializerCastRhs) :: Nil)
          .withRefEdges(patternInitializerCastRhs, lhsRefsTo.toList)

      val initializerAssignmentCall = newOperatorCallNode(
        Operators.assignment,
        s"$variableName = ${patternInitializerCast.code}",
        Option(variableType),
        line(typePatternExpr),
        column(typePatternExpr)
      )
      val initializerAssignmentAst = callAst(
        initializerAssignmentCall,
        Ast(patternIdentifier) :: initializerCastAst :: Nil
      ).withRefEdge(patternIdentifier, patternLocal)

      scope.enclosingMethod.foreach { methodScope =>
        methodScope.putPatternVariableInfo(typePatternExpr, patternLocal, initializerAssignmentAst)
      }
    }

    val instanceOfCall = newOperatorCallNode(
      Operators.instanceOf,
      s"${lhsAst.rootCodeOrEmpty} instanceof ${code(pattern.getType)}",
      Option(TypeConstants.Boolean)
    )

    callAst(instanceOfCall, lhsAst :: Ast(patternTypeRef) :: Nil)
  }

  private def getTypePatterns(expr: PatternExpr): List[TypePatternExpr] = {
    expr match {
      case typePatternExpr: TypePatternExpr => typePatternExpr :: Nil

      case recordPatternExpr: RecordPatternExpr =>
        recordPatternExpr.getPatternList.asScala.toList.flatMap(getTypePatterns)
    }
  }
}
