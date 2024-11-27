package io.joern.javasrc2cpg.astcreation.statements

import com.github.javaparser.ast.expr.{Expression, NameExpr}
import com.github.javaparser.ast.stmt.{BlockStmt, ForEachStmt, ForStmt}
import com.github.javaparser.symbolsolver.javaparsermodel.contexts.ForStatementContext
import com.github.javaparser.symbolsolver.resolution.typesolvers.CombinedTypeSolver
import io.joern.javasrc2cpg.astcreation.{AstCreator, ExpectedType}
import io.joern.javasrc2cpg.scope.NodeTypeInfo
import io.joern.javasrc2cpg.typesolvers.TypeInfoCalculator.TypeConstants
import io.joern.x2cpg.Ast
import io.joern.x2cpg.utils.IntervalKeyPool
import io.joern.x2cpg.utils.NodeBuilders.{newCallNode, newFieldIdentifierNode, newIdentifierNode, newOperatorCallNode}
import io.shiftleft.codepropertygraph.generated.nodes.Call.PropertyDefaults
import io.shiftleft.codepropertygraph.generated.nodes.{
  NewBlock,
  NewControlStructure,
  NewLiteral,
  NewLocal,
  NewMember,
  NewMethodParameterIn,
  NewNode
}
import io.shiftleft.codepropertygraph.generated.{ControlStructureTypes, DispatchTypes, Operators}
import org.slf4j.LoggerFactory

import scala.jdk.CollectionConverters.*
import scala.jdk.OptionConverters.RichOptional

trait AstForForLoopsCreator { this: AstCreator =>

  private val logger = LoggerFactory.getLogger(this.getClass())

  // TODO: Perhaps move this to a NameProvider or some such? Look at kt2cpg to see if some unified representation
  // makes sense.
  private val IndexNamePrefix    = "$idx"
  private val indexKeyPool       = new IntervalKeyPool(first = 0, last = Long.MaxValue)
  private val IterableNamePrefix = "$iterLocal"
  private val iterableKeyPool    = new IntervalKeyPool(first = 0, last = Long.MaxValue)

  private def nextIndexName(): String = {
    s"$IndexNamePrefix${indexKeyPool.next}"
  }

  private def nextIterableName(): String = {
    s"$IterableNamePrefix${iterableKeyPool.next}"
  }

  def astsForFor(stmt: ForStmt): List[Ast] = {
    val forContext = new ForStatementContext(stmt, new CombinedTypeSolver())

    val forNode =
      NewControlStructure()
        .controlStructureType(ControlStructureTypes.FOR)
        .code(getForCode(stmt))
        .lineNumber(line(stmt))
        .columnNumber(column(stmt))

    val initAsts =
      stmt.getInitialization.asScala.flatMap(astsForExpression(_, expectedType = ExpectedType.empty))

    val compareAsts = stmt.getCompare.toScala.toList.flatMap {
      astsForExpression(_, ExpectedType.Boolean)
    }

    val updateAsts = stmt.getUpdate.asScala.toList match {
      case Nil => Nil

      case expressions =>
        scope.pushBlockScope()
        scope.addLocalsForPatternsToEnclosingBlock(
          forContext.typePatternExprsExposedToChild(expressions.head).asScala.toList
        )
        val updateAsts = expressions.flatMap(astsForExpression(_, ExpectedType.empty))
        scope.popBlockScope()
        updateAsts
    }

    val patternPartition = partitionPatternAstsByScope(forContext)

    scope.pushBlockScope()
    scope.addLocalsForPatternsToEnclosingBlock(patternPartition.patternsIntroducedToBody)
    val bodyAst = wrapInBlockWithPrefix(patternPartition.astsAddedToBody, stmt.getBody)
    scope.popBlockScope()

    scope.addLocalsForPatternsToEnclosingBlock(patternPartition.patternsIntroducedByStatement)

    val ast = Ast(forNode)
      .withChildren(initAsts)
      .withChildren(compareAsts)
      .withChildren(updateAsts)
      .withChild(bodyAst)

    val astWithConditionEdge = compareAsts.flatMap(_.root) match {
      case c :: Nil =>
        ast.withConditionEdge(forNode, c)
      case _ => ast
    }

    patternPartition.astsAddedBeforeStatement ++ (astWithConditionEdge :: patternPartition.astsAddedAfterStatement)
  }

  def astForForEach(stmt: ForEachStmt): Seq[Ast] = {
    scope.pushBlockScope()

    // TODO: Does the type need to be registered here?
    val ast = expressionReturnTypeFullName(stmt.getIterable) match {
      case Some(typeFullName) if typeFullName.endsWith("[]") =>
        astsForNativeForEach(stmt, Some(typeFullName))

      case maybeType =>
        astForIterableForEach(stmt, maybeType)
    }

    scope.popBlockScope()
    ast
  }

  private def astForIterableForEach(stmt: ForEachStmt, iterableType: Option[String]): Seq[Ast] = {
    val lineNo = line(stmt)

    val iteratorLocalNode = iteratorLocalForForEach(lineNo)
    val iteratorAssignAst =
      iteratorAssignAstForForEach(stmt.getIterable, iteratorLocalNode, iterableType, lineNo)
    val iteratorHasNextCallAst = hasNextCallAstForForEach(iteratorLocalNode, lineNo)
    val variableLocal          = variableLocalForForEachBody(stmt)
    val variableAssignAst      = astForIterableForEachItemAssign(iteratorLocalNode, variableLocal)

    val bodyPrefixAsts = Seq(Ast(variableLocal), variableAssignAst)
    val bodyAst = stmt.getBody match {
      case block: BlockStmt =>
        astForBlockStatement(block, prefixAsts = bodyPrefixAsts)

      case bodyStmt =>
        val bodyBlockNode = NewBlock().lineNumber(lineNo)
        val bodyStmtAsts  = astsForStatement(bodyStmt)
        Ast(bodyBlockNode)
          .withChildren(bodyPrefixAsts)
          .withChildren(bodyStmtAsts)
    }

    val forNode =
      NewControlStructure()
        .controlStructureType(ControlStructureTypes.WHILE)
        .code(ControlStructureTypes.FOR)
        .lineNumber(lineNo)
        .columnNumber(column(stmt))

    val forAst = controlStructureAst(forNode, Some(iteratorHasNextCallAst), List(bodyAst))

    Seq(Ast(iteratorLocalNode), iteratorAssignAst, forAst)
  }

  private def astForIterableForEachItemAssign(iteratorLocalNode: NewLocal, variableLocal: NewLocal): Ast = {
    val lineNo          = variableLocal.lineNumber
    val forVariableType = variableLocal.typeFullName
    val varLocalAssignNode =
      newOperatorCallNode(Operators.assignment, PropertyDefaults.Code, Some(forVariableType), lineNo)
    val varLocalAssignIdentifier = newIdentifierNode(variableLocal.name, variableLocal.typeFullName)

    val iterNextCallNode =
      newCallNode(
        "next",
        Some(TypeConstants.Iterator),
        TypeConstants.Object,
        DispatchTypes.DYNAMIC_DISPATCH,
        lineNumber = lineNo
      )
    val iterNextCallReceiver = newIdentifierNode(iteratorLocalNode.name, iteratorLocalNode.typeFullName)
    val iterNextCallAst =
      callAst(iterNextCallNode, base = Some(Ast(iterNextCallReceiver)))
        .withRefEdge(iterNextCallReceiver, iteratorLocalNode)

    callAst(varLocalAssignNode, List(Ast(varLocalAssignIdentifier), iterNextCallAst))
      .withRefEdge(varLocalAssignIdentifier, variableLocal)
  }

  private def astsForNativeForEach(stmt: ForEachStmt, iterableType: Option[String]): Seq[Ast] = {

    // This is ugly, but for a case like `for (int x : new int[] { ... })` this creates a new LOCAL
    // with the assignment `int[] $iterLocal0 = new int[] { ... }` before the FOR loop.
    // TODO: Fix this
    val (iterableSource: NodeTypeInfo, tempIterableInitAsts) = stmt.getIterable match {
      case nameExpr: NameExpr =>
        scope.lookupVariable(nameExpr.getNameAsString).asNodeInfoOption match {
          // If this is not the case, then the code is broken (iterable not in scope).
          case Some(nodeTypeInfo) => (nodeTypeInfo, Nil)
          case _                  => iterableAssignAstsForNativeForEach(nameExpr, iterableType)
        }
      case iterableExpr => iterableAssignAstsForNativeForEach(iterableExpr, iterableType)
    }

    val forNode = NewControlStructure()
      .controlStructureType(ControlStructureTypes.FOR)

    val lineNo = line(stmt)

    val idxLocal          = nativeForEachIdxLocalNode(lineNo)
    val idxInitializerAst = nativeForEachIdxInitializerAst(lineNo, idxLocal)
    // TODO next: pass NodeTypeInfo around
    val compareAst   = nativeForEachCompareAst(lineNo, iterableSource, idxLocal)
    val incrementAst = nativeForEachIncrementAst(lineNo, idxLocal)
    val bodyAst      = nativeForEachBodyAst(stmt, idxLocal, iterableSource)

    val forAst = Ast(forNode)
      .withChild(Ast(idxLocal))
      .withChild(idxInitializerAst)
      .withChild(compareAst)
      .withChild(incrementAst)
      .withChild(bodyAst)
      .withConditionEdges(forNode, compareAst.root.toList)

    tempIterableInitAsts ++ Seq(forAst)
  }

  private def iterableAssignAstsForNativeForEach(
    iterableExpression: Expression,
    iterableType: Option[String]
  ): (NodeTypeInfo, Seq[Ast]) = {
    val lineNo       = line(iterableExpression)
    val expectedType = ExpectedType(iterableType)

    val iterableAst = astsForExpression(iterableExpression, expectedType = expectedType) match {
      case Nil =>
        logger.error(s"Could not create AST for iterable expr $iterableExpression: $filename:l$lineNo")
        Ast()
      case iterableAstHead :: Nil => iterableAstHead
      case iterableAsts =>
        logger.warn(
          s"Found multiple ASTS for iterable expr $iterableExpression: $filename:l$lineNo\nDropping all but the first!"
        )
        iterableAsts.head
    }

    val iterableName      = nextIterableName()
    val iterableLocalNode = localNode(iterableExpression, iterableName, iterableName, iterableType.getOrElse("ANY"))
    val iterableLocalAst  = Ast(iterableLocalNode)

    val iterableAssignNode =
      newOperatorCallNode(Operators.assignment, code = "", line = lineNo, typeFullName = iterableType)
    val iterableAssignIdentifier =
      identifierNode(iterableExpression, iterableName, iterableName, iterableType.getOrElse("ANY"))
    val iterableAssignArgs = List(Ast(iterableAssignIdentifier), iterableAst)
    val iterableAssignAst =
      callAst(iterableAssignNode, iterableAssignArgs)
        .withRefEdge(iterableAssignIdentifier, iterableLocalNode)

    (
      NodeTypeInfo(iterableLocalNode, iterableLocalNode.name, Some(iterableLocalNode.typeFullName)),
      List(iterableLocalAst, iterableAssignAst)
    )
  }

  private def nativeForEachIdxLocalNode(lineNo: Option[Int]): NewLocal = {
    val idxName      = nextIndexName()
    val typeFullName = TypeConstants.Int
    val idxLocal =
      NewLocal()
        .name(idxName)
        .typeFullName(typeFullName)
        .code(idxName)
        .lineNumber(lineNo)
    scope.enclosingBlock.get.addLocal(idxLocal)
    idxLocal
  }

  private def nativeForEachIdxInitializerAst(lineNo: Option[Int], idxLocal: NewLocal): Ast = {
    val idxName = idxLocal.name
    val idxInitializerCallNode = newOperatorCallNode(
      Operators.assignment,
      code = s"int $idxName = 0",
      line = lineNo,
      typeFullName = Some(TypeConstants.Int)
    )
    val idxIdentifierArg = newIdentifierNode(idxName, idxLocal.typeFullName)
    val zeroLiteral =
      NewLiteral()
        .code("0")
        .typeFullName(TypeConstants.Int)
        .lineNumber(lineNo)
    val idxInitializerArgAsts = List(Ast(idxIdentifierArg), Ast(zeroLiteral))
    callAst(idxInitializerCallNode, idxInitializerArgAsts)
      .withRefEdge(idxIdentifierArg, idxLocal)
  }

  private def nativeForEachCompareAst(lineNo: Option[Int], iterableSource: NodeTypeInfo, idxLocal: NewLocal): Ast = {
    val idxName = idxLocal.name

    val compareNode = newOperatorCallNode(
      Operators.lessThan,
      code = s"$idxName < ${iterableSource.name}.length",
      typeFullName = Some(TypeConstants.Boolean),
      line = lineNo
    )
    val comparisonIdxIdentifier = newIdentifierNode(idxName, idxLocal.typeFullName)
    val comparisonFieldAccess = newOperatorCallNode(
      Operators.fieldAccess,
      code = s"${iterableSource.name}.length",
      typeFullName = Some(TypeConstants.Int),
      line = lineNo
    )
    val fieldAccessIdentifier = newIdentifierNode(iterableSource.name, iterableSource.typeFullName.getOrElse("ANY"))
    val fieldAccessFieldIdentifier = newFieldIdentifierNode("length", lineNo)
    val fieldAccessArgs            = List(fieldAccessIdentifier, fieldAccessFieldIdentifier).map(Ast(_))
    val fieldAccessAst             = callAst(comparisonFieldAccess, fieldAccessArgs)
    val compareArgs                = List(Ast(comparisonIdxIdentifier), fieldAccessAst)

    // TODO: This is a workaround for a crash when looping over statically imported members. Handle those properly.
    val iterableSourceNode = localParamOrMemberFromNode(iterableSource)

    callAst(compareNode, compareArgs)
      .withRefEdge(comparisonIdxIdentifier, idxLocal)
      .withRefEdges(fieldAccessIdentifier, iterableSourceNode.toList)
  }

  private def nativeForEachIncrementAst(lineNo: Option[Int], idxLocal: NewLocal): Ast = {
    val incrementNode = newOperatorCallNode(
      Operators.postIncrement,
      code = s"${idxLocal.name}++",
      typeFullName = Some(TypeConstants.Int),
      line = lineNo
    )
    val incrementArg    = newIdentifierNode(idxLocal.name, idxLocal.typeFullName)
    val incrementArgAst = Ast(incrementArg)
    callAst(incrementNode, List(incrementArgAst))
      .withRefEdge(incrementArg, idxLocal)
  }

  private def variableLocalForForEachBody(stmt: ForEachStmt): NewLocal = {
    val lineNo = line(stmt)
    // Create item local
    val maybeVariable = stmt.getVariable.getVariables.asScala.toList match {
      case Nil =>
        logger.error(s"ForEach statement has empty variable list: $filename$lineNo")
        None
      case variable :: Nil => Some(variable)
      case variable :: _ =>
        logger.warn(s"ForEach statement defines multiple variables. Dropping all but the first: $filename$lineNo")
        Some(variable)
    }

    val partialLocalNode = NewLocal().lineNumber(lineNo)

    maybeVariable match {
      case Some(variable) =>
        val name = variable.getNameAsString
        val typeFullName =
          tryWithSafeStackOverflow(variable.getType).toOption.flatMap(typeInfoCalc.fullName).getOrElse("ANY")
        val localNode = partialLocalNode
          .name(name)
          .code(variable.getNameAsString)
          .typeFullName(typeFullName)

        scope.enclosingBlock.get.addLocal(localNode)
        localNode

      case None =>
        // Returning partialLocalNode here is fine since getting to this case means everything is broken anyways :)
        partialLocalNode
    }
  }

  private def iteratorLocalForForEach(lineNumber: Option[Int]): NewLocal = {
    val iteratorLocalName = nextIterableName()
    NewLocal()
      .name(iteratorLocalName)
      .code(iteratorLocalName)
      .typeFullName(TypeConstants.Iterator)
      .lineNumber(lineNumber)
  }

  private def iteratorAssignAstForForEach(
    iterExpr: Expression,
    iteratorLocalNode: NewLocal,
    iterableType: Option[String],
    lineNo: Option[Int]
  ): Ast = {
    val iteratorAssignNode =
      newOperatorCallNode(Operators.assignment, code = "", typeFullName = Some(TypeConstants.Iterator), line = lineNo)
    val iteratorAssignIdentifier =
      identifierNode(iterExpr, iteratorLocalNode.name, iteratorLocalNode.name, iteratorLocalNode.typeFullName)

    val iteratorCallNode =
      newCallNode("iterator", iterableType, TypeConstants.Iterator, DispatchTypes.DYNAMIC_DISPATCH, lineNumber = lineNo)

    val actualIteratorAst = astsForExpression(iterExpr, expectedType = ExpectedType.empty).toList match {
      case Nil =>
        logger.warn(s"Could not create receiver ast for iterator $iterExpr")
        None

      case ast :: Nil => Some(ast)

      case ast :: _ =>
        logger.warn(s"Created multiple receiver asts for $iterExpr. Dropping all but the first.")
        Some(ast)
    }

    val iteratorCallAst =
      callAst(iteratorCallNode, base = actualIteratorAst)

    callAst(iteratorAssignNode, List(Ast(iteratorAssignIdentifier), iteratorCallAst))
      .withRefEdge(iteratorAssignIdentifier, iteratorLocalNode)
  }

  private def hasNextCallAstForForEach(iteratorLocalNode: NewLocal, lineNo: Option[Int]): Ast = {
    val iteratorHasNextCallNode =
      newCallNode(
        "hasNext",
        Some(TypeConstants.Iterator),
        TypeConstants.Boolean,
        DispatchTypes.DYNAMIC_DISPATCH,
        lineNumber = lineNo
      )
    val iteratorHasNextCallReceiver =
      newIdentifierNode(iteratorLocalNode.name, iteratorLocalNode.typeFullName)

    callAst(iteratorHasNextCallNode, base = Some(Ast(iteratorHasNextCallReceiver)))
      .withRefEdge(iteratorHasNextCallReceiver, iteratorLocalNode)
  }

  private def variableAssignForNativeForEachBody(
    variableLocal: NewLocal,
    idxLocal: NewLocal,
    iterable: NodeTypeInfo
  ): Ast = {
    // Everything will be on the same line as the `for` statement, but this is the most useful
    // solution for debugging.
    val lineNo = variableLocal.lineNumber
    val varAssignNode =
      newOperatorCallNode(Operators.assignment, PropertyDefaults.Code, Some(variableLocal.typeFullName), lineNo)

    val targetNode = newIdentifierNode(variableLocal.name, variableLocal.typeFullName)

    val indexAccessTypeFullName = iterable.typeFullName.map(_.replaceAll(raw"\[]", ""))
    val indexAccess =
      newOperatorCallNode(Operators.indexAccess, PropertyDefaults.Code, indexAccessTypeFullName, lineNo)

    val indexAccessIdentifier = newIdentifierNode(iterable.name, iterable.typeFullName.getOrElse("ANY"))
    val indexAccessIndex      = newIdentifierNode(idxLocal.name, idxLocal.typeFullName)

    val indexAccessArgsAsts = List(indexAccessIdentifier, indexAccessIndex).map(Ast(_))
    val indexAccessAst      = callAst(indexAccess, indexAccessArgsAsts)

    val iterableSourceNode = localParamOrMemberFromNode(iterable)

    val assignArgsAsts = List(Ast(targetNode), indexAccessAst)
    callAst(varAssignNode, assignArgsAsts)
      .withRefEdge(targetNode, variableLocal)
      .withRefEdges(indexAccessIdentifier, iterableSourceNode.toList)
      .withRefEdge(indexAccessIndex, idxLocal)
  }

  private def nativeForEachBodyAst(stmt: ForEachStmt, idxLocal: NewLocal, iterable: NodeTypeInfo): Ast = {
    val variableLocal     = variableLocalForForEachBody(stmt)
    val variableLocalAst  = Ast(variableLocal)
    val variableAssignAst = variableAssignForNativeForEachBody(variableLocal, idxLocal, iterable)

    stmt.getBody match {
      case block: BlockStmt =>
        astForBlockStatement(block, prefixAsts = List(variableLocalAst, variableAssignAst))
      case statement =>
        val stmtAsts  = astsForStatement(statement)
        val blockNode = NewBlock().lineNumber(variableLocal.lineNumber)
        Ast(blockNode)
          .withChild(variableLocalAst)
          .withChild(variableAssignAst)
          .withChildren(stmtAsts)
    }
  }

  private def localParamOrMemberFromNode(nodeTypeInfo: NodeTypeInfo): Option[NewNode] = {
    nodeTypeInfo.node match {
      case localNode: NewLocal                 => Some(localNode)
      case memberNode: NewMember               => Some(memberNode)
      case parameterNode: NewMethodParameterIn => Some(parameterNode)
      case _                                   => None
    }
  }

  private def getForCode(stmt: ForStmt): String = {
    val init    = stmt.getInitialization.asScala.map(_.toString).mkString(", ")
    val compare = stmt.getCompare.toScala.map(_.toString)
    val update  = stmt.getUpdate.asScala.map(_.toString).mkString(", ")
    s"for ($init; $compare; $update)"
  }
}
