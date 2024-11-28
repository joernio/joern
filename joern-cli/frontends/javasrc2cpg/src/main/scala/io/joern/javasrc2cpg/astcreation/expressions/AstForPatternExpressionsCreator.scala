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
import io.joern.x2cpg.{Ast, Defines}
import io.joern.x2cpg.utils.AstPropertiesUtil.*
import io.shiftleft.codepropertygraph.generated.nodes.{AstNodeNew, NewIdentifier}
import io.joern.x2cpg.utils.NodeBuilders.*
import io.shiftleft.codepropertygraph.generated.Operators

import scala.jdk.CollectionConverters.*
import scala.jdk.OptionConverters.RichOptional

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

  private def createAndPushAssignmentForTypePattern(typePatternExpr: TypePatternExpr, castRhs: Ast): Unit = {
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
    val variableTypeCode           = tryWithSafeStackOverflow(code(typePatternExpr.getType)).getOrElse(variableType)
    val patternLocal               = localNode(typePatternExpr, variableName, code(typePatternExpr), variableType)
    val patternIdentifier          = identifierNode(typePatternExpr, variableName, variableName, variableType)
    val patternInitializerCastType = typeRefNode(typePatternExpr, code(typePatternExpr.getType), variableType)
    val patternInitializerCast = newOperatorCallNode(
      Operators.cast,
      s"($variableTypeCode) ${castRhs.rootCodeOrEmpty}",
      Option(variableType),
      line(typePatternExpr),
      column(typePatternExpr)
    )

    val initializerCastAst = callAst(patternInitializerCast, Ast(patternInitializerCastType) :: castRhs :: Nil)

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

  private def createAndPushAssignmentAstsForPattern(
    patternExpr: PatternExpr,
    castRhsIdentifier: Option[NewIdentifier],
    castRhs: Ast
  ): Unit = {
    patternExpr match {
      case typePatternExpr: TypePatternExpr =>
        createAndPushAssignmentForTypePattern(typePatternExpr, castRhs)

      case recordPatternExpr: RecordPatternExpr =>
        val recordType = tryWithSafeStackOverflow(recordPatternExpr.getType).toOption
        val resolvedRecordType =
          recordType.flatMap(typ => tryWithSafeStackOverflow(typ.resolve().asReferenceType()).toOption)

        val recordTypeFullName = recordType
          .map(typ =>
            scope.lookupType(code(typ)).orElse(typeInfoCalc.fullName(typ)).getOrElse(defaultTypeFallback(typ))
          )
          .getOrElse(defaultTypeFallback())

        val castType = typeRefNode(recordPatternExpr, code(recordPatternExpr.getType), recordTypeFullName)
        val castNode = newOperatorCallNode(
          Operators.cast,
          s"(${castType.code}) ${castRhs.rootCodeOrEmpty}",
          Option(recordTypeFullName)
        )
        val castAst = callAst(castNode, Ast(castType) :: castRhs :: Nil)

        val patternList = recordPatternExpr.getPatternList.asScala.toList
        val fieldNames = resolvedRecordType
          .flatMap(_.getTypeDeclaration.toScala)
          .map(_.getDeclaredFields.asScala.map(_.getName).toList)
          .getOrElse(patternList.map(_ => Defines.UnknownField))

        patternList.zip(fieldNames).zipWithIndex.foreach { case ((patternExpr, fieldName), idx) =>
          val patternTypeFullName = tryWithSafeStackOverflow(patternExpr.getType).toOption
            .map { typ =>
              scope
                .lookupScopeType(typ.asString())
                .map(_.typeFullName)
                .orElse(typeInfoCalc.fullName(typ))
                .getOrElse(defaultTypeFallback(typ))
            }
            .getOrElse(defaultTypeFallback())

          val fieldIdentifier = fieldIdentifierNode(patternExpr, fieldName, fieldName)
          val fieldAccessCall =
            newOperatorCallNode(
              Operators.fieldAccess,
              s"(${castAst.rootCodeOrEmpty}).$fieldName",
              Option(patternTypeFullName)
            )

          val lhsAst =
            if (idx == 0)
              castAst
            else
              castAst.subTreeCopy(castAst.root.collect { case astRoot: AstNodeNew => astRoot }.get)

          val fieldAccessAst = callAst(fieldAccessCall, lhsAst :: Ast(fieldIdentifier) :: Nil)

          createAndPushAssignmentAstsForPattern(patternExpr, None, fieldAccessAst)
        }
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
    createAndPushAssignmentAstsForPattern(pattern, Option(lhsIdentifier), lhsAst)

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
