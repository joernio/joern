package io.joern.javasrc2cpg.util

import com.github.javaparser.ast.{Node, NodeList}
import com.github.javaparser.ast.body.{FieldDeclaration, MethodDeclaration, Parameter, TypeDeclaration}
import com.github.javaparser.ast.expr.{
  ConditionalExpr,
  LambdaExpr,
  MethodCallExpr,
  NameExpr,
  ObjectCreationExpr,
  SuperExpr,
  SwitchExpr,
  ThisExpr,
  TypePatternExpr,
  VariableDeclarationExpr
}
import com.github.javaparser.ast.stmt.{BlockStmt, IfStmt, LocalClassDeclarationStmt, SwitchStmt, TryStmt}
import com.github.javaparser.symbolsolver.javaparsermodel.contexts.{
  ConditionalExprContext,
  IfStatementContext,
  SwitchEntryContext
}
import com.github.javaparser.symbolsolver.resolution.typesolvers.CombinedTypeSolver
import io.joern.javasrc2cpg.scope.Scope

import java.util
import scala.collection.mutable
import scala.jdk.CollectionConverters.*
import scala.jdk.OptionConverters.*
import scala.util.Try

class CaptureUseFinder {

  private val cache: mutable.Map[Node, Set[String]] = mutable.Map()
  // Not actually used, but is needed as an argument to the context constructors
  private val typeSolver = new CombinedTypeSolver()

  def getCaptureUses(node: Node): Set[String] = {
    cache.get(node) match {
      case Some(captureUses) => captureUses

      case None =>
        val captureUses = mutable.Set[String]()
        storeCaptureUses(node, captureUses)
        captureUses.toSet
    }
  }

  // This method does a bottom-up traversal of the given AST and adds all variable names not defined within that AST
  // (i.e. captured from scope surrounding this subtree). When it encounters an identifier, that identifier
  // is added to the captureUses set
  private def storeCaptureUses(node: Node, captureUses: mutable.Set[String]): Unit = {
    cache.get(node) match {
      case Some(cachedCaptureUses) => captureUses.addAll(cachedCaptureUses)

      case None =>
        node match {
          case nameExpr: NameExpr =>
            captureUses.add(nameExpr.getNameAsString)

          case _: ThisExpr =>
            captureUses.add(NameConstants.This)

          case _: SuperExpr =>
            captureUses.add(NameConstants.This)

          case variableDeclarationExpr: VariableDeclarationExpr =>
            variableDeclarationExpr.getVariables.asScala.reverseIterator.foreach { variableDeclarator =>
              variableDeclarator.getInitializer.toScala.foreach(storeCaptureUses(_, captureUses))
              captureUses.remove(variableDeclarator.getNameAsString)
            }

          case parameter: Parameter =>
            captureUses.remove(parameter.getNameAsString)

          case ifStmt: IfStmt =>
            val context = IfStatementContext(ifStmt, typeSolver)

            val maybeElse             = ifStmt.getElseStmt.toScala
            val patternsExposedToElse = maybeElse.map(context.typePatternExprsExposedToChild(_).asScala).getOrElse(Nil)

            storeCaptureUsesForIfTypeNode(
              ifStmt.getThenStmt,
              maybeElse,
              ifStmt.getCondition,
              context.typePatternExprsExposedToChild(ifStmt.getThenStmt).asScala,
              patternsExposedToElse,
              captureUses
            )

            context.getIntroducedTypePatterns.forEach(tpe => captureUses.remove(tpe.getNameAsString))

          case conditionalExpr: ConditionalExpr =>
            val context = ConditionalExprContext(conditionalExpr, typeSolver)

            storeCaptureUsesForIfTypeNode(
              conditionalExpr.getThenExpr,
              Option(conditionalExpr.getElseExpr),
              conditionalExpr.getCondition,
              context.typePatternExprsExposedToChild(conditionalExpr.getThenExpr).asScala,
              context.typePatternExprsExposedToChild(conditionalExpr.getElseExpr).asScala,
              captureUses
            )

          case switchNode: (SwitchStmt | SwitchExpr) =>
            storeCaptureUses(switchNode.getSelector, captureUses)

            switchNode.getEntries.forEach { switchEntry =>
              val context = SwitchEntryContext(switchEntry, typeSolver)

              val entryCaptureUses = mutable.Set[String]()

              switchEntry.getStatements.asScala.reverseIterator.foreach(storeCaptureUses(_, entryCaptureUses))
              switchEntry.getGuard.toScala.foreach(storeCaptureUses(_, entryCaptureUses))

              switchEntry.getStatements.getFirst.toScala.foreach { firstStatement =>
                context
                  .typePatternExprsExposedToChild(firstStatement)
                  .forEach(tpe => entryCaptureUses.remove(tpe.getNameAsString))
              }

              switchEntry.getLabels.forEach(storeCaptureUses(_, entryCaptureUses))

              captureUses.addAll(entryCaptureUses)
            }

          case methodCall: MethodCallExpr =>
            val isStatic = Try(methodCall.resolve()).toOption.exists(_.isStatic)
            // If it's a non-static call without an explicit receiver, then either it's a call to `this.methodName`, or
            // TODO it's a call to a statically imported method which must be handled properly. For now an extra `this`
            //  local may be created
            if (!isStatic && methodCall.getScope.isEmpty) {
              captureUses.add(NameConstants.This)
            }

            methodCall.getChildNodes.asScala.reverseIterator.foreach(storeCaptureUses(_, captureUses))

          case objectCreationExpr: ObjectCreationExpr =>
            objectCreationExpr.getAnonymousClassBody.toScala.match {
              case Some(bodyElements) =>
                val anonymousClassCaptureUses = mutable.Set[String]()
                bodyElements.asScala.reverseIterator.foreach(storeCaptureUses(_, anonymousClassCaptureUses))
                // unhandled (up to this point) `this` references in the body refer to the anonymous class instance
                anonymousClassCaptureUses.remove(NameConstants.This)
                captureUses.addAll(anonymousClassCaptureUses)

              case None => // Nothing to do here
            }

            objectCreationExpr.getArguments.forEach(storeCaptureUses(_, captureUses))
            objectCreationExpr.getScope.toScala.foreach(storeCaptureUses(_, captureUses))

          case tryStmt: TryStmt =>
            val tryCaptureUses = mutable.Set[String]()
            tryStmt.getFinallyBlock.toScala.foreach(storeCaptureUses(_, tryCaptureUses))

            tryStmt.getCatchClauses.asScala.reverseIterator.foreach { catchClause =>
              val catchClauseCaptureUses = mutable.Set[String]()
              catchClause.getChildNodes.forEach(storeCaptureUses(_, catchClauseCaptureUses))
              captureUses.addAll(catchClauseCaptureUses)
            }

            storeCaptureUses(tryStmt.getTryBlock, tryCaptureUses)

            tryStmt.getResources.asScala.reverseIterator.foreach(storeCaptureUses(_, tryCaptureUses))

            captureUses.addAll(tryCaptureUses)

          case nodeIntroducingNewScope: (BlockStmt | MethodDeclaration | TypeDeclaration[?] | LambdaExpr) =>
            val newScopeCaptureUses = mutable.Set[String]()
            nodeIntroducingNewScope.getChildNodes.asScala.reverseIterator
              .foreach(storeCaptureUses(_, newScopeCaptureUses))

            nodeIntroducingNewScope match {
              case _: BlockStmt =>
              // Don't cache blocks since they will usually be the top level in a method/type/lambda body

              case _ =>
                cache.put(nodeIntroducingNewScope, newScopeCaptureUses.toSet)
            }

            captureUses.addAll(newScopeCaptureUses)

            nodeIntroducingNewScope match {
              case methodDeclaration: MethodDeclaration if !methodDeclaration.isStatic =>
                captureUses.remove(NameConstants.This)

              case _: TypeDeclaration[?] =>
                // Type declarations (classes, anonymous classes, local classes) provide their own `this`
                captureUses.remove(NameConstants.This)

              case _ => // Nothing to do here
            }

          case _ =>
            node.getChildNodes.asScala.reverseIterator.foreach(storeCaptureUses(_, captureUses))
        }
    }
  }

  private def storeCaptureUsesForIfTypeNode(
    thenNode: Node,
    maybeElseNode: Option[Node],
    conditionNode: Node,
    patternsIntroducedToThen: Iterable[TypePatternExpr],
    patternsIntroducedToElse: Iterable[TypePatternExpr],
    captureUses: mutable.Set[String]
  ): Unit = {

    val thenCaptureUses = mutable.Set[String]()
    storeCaptureUses(thenNode, thenCaptureUses)
    patternsIntroducedToThen.foreach(tpe => thenCaptureUses.remove(tpe.getNameAsString))

    val elseCaptureUses = mutable.Set[String]()
    maybeElseNode.foreach { elseNode =>
      storeCaptureUses(elseNode, elseCaptureUses)
      patternsIntroducedToElse.foreach(tpe => elseCaptureUses.remove(tpe.getNameAsString))
    }

    storeCaptureUses(conditionNode, captureUses)

    captureUses.addAll(thenCaptureUses)
    captureUses.addAll(elseCaptureUses)
  }
}
