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

/** The CaptureUseFinder does a conceptually bottom-up (practically top-down post-order reverse) traversal of the given
  * AST and adds all variable names not defined within that AST (i.e. captured from scope surrounding this subtree).
  * This effectively means traversing the source code in reverse starting at the end of the given structure. When it
  * encounters an identifier, that identifier is added to the undeclaredVariables set since a corresponding declaration
  * could not have been seen yet. Then, when a declaration is seen, the matching name is removed from undeclared
  * variables since the corresponding NameExpr is no longer capturing anything from the already processed scope.
  *
  * The actual implementation is a bit muddier due to the use of mutable sets to avoid the performance cost of
  * constantly creating and merging new sets. This means that at some pre-order points, new sets for undeclared
  * variables need to be created to handle nodes that create new scope (blocks, methods, etc.). Once these subtrees have
  * been fully processed, these temporary sets are merged into the main set keeping track of undeclared variables.
  *
  * Once a node declaring a new type has been processed, the results are cached, so a given file should only be
  * traversed once
  */
class CaptureUseFinder {

  private val cache: mutable.Map[Node, Set[String]] = mutable.Map()
  // Not actually used, but is needed as an argument to the context constructors
  private val typeSolver = new CombinedTypeSolver()

  def getUndeclaredVariables(node: Node): Set[String] = {
    cache.get(node) match {
      case Some(undeclaredVariables) => undeclaredVariables

      case None =>
        val undeclaredVariables = mutable.Set[String]()
        findUndeclaredVariablesInSubtree(node, undeclaredVariables)
        undeclaredVariables.toSet
    }
  }

  private def findUndeclaredVariablesInSubtree(node: Node, undeclaredVariables: mutable.Set[String]): Unit = {
    cache.get(node) match {
      case Some(cachedUndeclaredVariables) => undeclaredVariables.addAll(cachedUndeclaredVariables)

      case None =>
        node match {
          case nameExpr: NameExpr =>
            undeclaredVariables.add(nameExpr.getNameAsString)

          case _: ThisExpr =>
            undeclaredVariables.add(NameConstants.This)

          case _: SuperExpr =>
            undeclaredVariables.add(NameConstants.This)

          case variableDeclarationExpr: VariableDeclarationExpr =>
            variableDeclarationExpr.getVariables.asScala.reverseIterator.foreach { variableDeclarator =>
              variableDeclarator.getInitializer.toScala.foreach(
                findUndeclaredVariablesInSubtree(_, undeclaredVariables)
              )
              undeclaredVariables.remove(variableDeclarator.getNameAsString)
            }

          case parameter: Parameter =>
            undeclaredVariables.remove(parameter.getNameAsString)

          case ifStmt: IfStmt =>
            val context = IfStatementContext(ifStmt, typeSolver)

            val maybeElse             = ifStmt.getElseStmt.toScala
            val patternsExposedToElse = maybeElse.map(context.typePatternExprsExposedToChild(_).asScala).getOrElse(Nil)

            storeUndeclaredVariablesForIfTypeNode(
              ifStmt.getThenStmt,
              maybeElse,
              ifStmt.getCondition,
              context.typePatternExprsExposedToChild(ifStmt.getThenStmt).asScala,
              patternsExposedToElse,
              undeclaredVariables
            )

            context.getIntroducedTypePatterns.forEach(tpe => undeclaredVariables.remove(tpe.getNameAsString))

          case conditionalExpr: ConditionalExpr =>
            val context = ConditionalExprContext(conditionalExpr, typeSolver)

            storeUndeclaredVariablesForIfTypeNode(
              conditionalExpr.getThenExpr,
              Option(conditionalExpr.getElseExpr),
              conditionalExpr.getCondition,
              context.typePatternExprsExposedToChild(conditionalExpr.getThenExpr).asScala,
              context.typePatternExprsExposedToChild(conditionalExpr.getElseExpr).asScala,
              undeclaredVariables
            )

          case switchNode: (SwitchStmt | SwitchExpr) =>
            findUndeclaredVariablesInSubtree(switchNode.getSelector, undeclaredVariables)

            switchNode.getEntries.forEach { switchEntry =>
              val context = SwitchEntryContext(switchEntry, typeSolver)

              val entryUndeclaredVariables = mutable.Set[String]()

              switchEntry.getStatements.asScala.reverseIterator
                .foreach(findUndeclaredVariablesInSubtree(_, entryUndeclaredVariables))
              switchEntry.getGuard.toScala.foreach(findUndeclaredVariablesInSubtree(_, entryUndeclaredVariables))

              switchEntry.getStatements.getFirst.toScala.foreach { firstStatement =>
                context
                  .typePatternExprsExposedToChild(firstStatement)
                  .forEach(tpe => entryUndeclaredVariables.remove(tpe.getNameAsString))
              }

              switchEntry.getLabels.forEach(findUndeclaredVariablesInSubtree(_, entryUndeclaredVariables))

              undeclaredVariables.addAll(entryUndeclaredVariables)
            }

          case methodCall: MethodCallExpr =>
            val isStatic = Try(methodCall.resolve()).toOption.exists(_.isStatic)
            // If it's a non-static call without an explicit receiver, then either it's a call to `this.methodName`, or
            // TODO it's a call to a statically imported method which must be handled properly. For now an extra `this`
            //  local may be created
            if (!isStatic && methodCall.getScope.isEmpty) {
              undeclaredVariables.add(NameConstants.This)
            }

            methodCall.getChildNodes.asScala.reverseIterator
              .foreach(findUndeclaredVariablesInSubtree(_, undeclaredVariables))

          case objectCreationExpr: ObjectCreationExpr =>
            objectCreationExpr.getAnonymousClassBody.toScala.match {
              case Some(bodyElements) =>
                val anonymousClassUndeclaredVariables = mutable.Set[String]()
                bodyElements.asScala.reverseIterator.foreach(
                  findUndeclaredVariablesInSubtree(_, anonymousClassUndeclaredVariables)
                )
                // unhandled (up to this point) `this` references in the body refer to the anonymous class instance
                anonymousClassUndeclaredVariables.remove(NameConstants.This)
                undeclaredVariables.addAll(anonymousClassUndeclaredVariables)
                cache.put(objectCreationExpr, undeclaredVariables.toSet)

              case None => // Nothing to do here
            }

            objectCreationExpr.getArguments.forEach(findUndeclaredVariablesInSubtree(_, undeclaredVariables))
            objectCreationExpr.getScope.toScala.foreach(findUndeclaredVariablesInSubtree(_, undeclaredVariables))

          case tryStmt: TryStmt =>
            val tryUndeclaredVariables = mutable.Set[String]()
            tryStmt.getFinallyBlock.toScala.foreach(findUndeclaredVariablesInSubtree(_, tryUndeclaredVariables))

            tryStmt.getCatchClauses.asScala.reverseIterator.foreach { catchClause =>
              val catchClauseUndeclaredVariables = mutable.Set[String]()
              catchClause.getChildNodes.forEach(findUndeclaredVariablesInSubtree(_, catchClauseUndeclaredVariables))
              undeclaredVariables.addAll(catchClauseUndeclaredVariables)
            }

            findUndeclaredVariablesInSubtree(tryStmt.getTryBlock, tryUndeclaredVariables)

            tryStmt.getResources.asScala.reverseIterator
              .foreach(findUndeclaredVariablesInSubtree(_, tryUndeclaredVariables))

            undeclaredVariables.addAll(tryUndeclaredVariables)

          case nodeIntroducingNewScope: (BlockStmt | MethodDeclaration | TypeDeclaration[?] | LambdaExpr) =>
            val newScopeUndeclaredVariables = mutable.Set[String]()
            nodeIntroducingNewScope.getChildNodes.asScala.reverseIterator
              .foreach(findUndeclaredVariablesInSubtree(_, newScopeUndeclaredVariables))

            nodeIntroducingNewScope match {
              case _: BlockStmt =>
              // Don't cache blocks since they will usually be the top level in a method/type/lambda body

              case _ =>
                cache.put(nodeIntroducingNewScope, newScopeUndeclaredVariables.toSet)
            }

            undeclaredVariables.addAll(newScopeUndeclaredVariables)

            nodeIntroducingNewScope match {
              case methodDeclaration: MethodDeclaration if !methodDeclaration.isStatic =>
                undeclaredVariables.remove(NameConstants.This)

              case _: TypeDeclaration[?] =>
                // Type declarations (classes, anonymous classes, local classes) provide their own `this`
                undeclaredVariables.remove(NameConstants.This)

              case _ => // Nothing to do here
            }

          case _ =>
            node.getChildNodes.asScala.reverseIterator.foreach(findUndeclaredVariablesInSubtree(_, undeclaredVariables))
        }
    }
  }

  private def storeUndeclaredVariablesForIfTypeNode(
    thenNode: Node,
    maybeElseNode: Option[Node],
    conditionNode: Node,
    patternsIntroducedToThen: Iterable[TypePatternExpr],
    patternsIntroducedToElse: Iterable[TypePatternExpr],
    undeclaredVariables: mutable.Set[String]
  ): Unit = {

    val thenUndeclaredVariables = mutable.Set[String]()
    findUndeclaredVariablesInSubtree(thenNode, thenUndeclaredVariables)
    patternsIntroducedToThen.foreach(tpe => thenUndeclaredVariables.remove(tpe.getNameAsString))

    val elseUndeclaredVariables = mutable.Set[String]()
    maybeElseNode.foreach { elseNode =>
      findUndeclaredVariablesInSubtree(elseNode, elseUndeclaredVariables)
      patternsIntroducedToElse.foreach(tpe => elseUndeclaredVariables.remove(tpe.getNameAsString))
    }

    findUndeclaredVariablesInSubtree(conditionNode, undeclaredVariables)

    undeclaredVariables.addAll(thenUndeclaredVariables)
    undeclaredVariables.addAll(elseUndeclaredVariables)
  }
}
