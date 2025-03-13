package io.joern.javasrc2cpg.astcreation.statements

import com.github.javaparser.ast.expr.{Expression, NameExpr}
import com.github.javaparser.ast.stmt.{BlockStmt, ForEachStmt, ForStmt}
import com.github.javaparser.symbolsolver.javaparsermodel.contexts.ForStatementContext
import com.github.javaparser.symbolsolver.resolution.typesolvers.CombinedTypeSolver
import io.joern.javasrc2cpg.astcreation.{AstCreator, ExpectedType}
import io.joern.javasrc2cpg.scope.NodeTypeInfo
import io.joern.javasrc2cpg.typesolvers.TypeInfoCalculator.TypeConstants
import io.joern.javasrc2cpg.util.NameConstants
import io.joern.javasrc2cpg.util.Util.composeMethodFullName
import io.joern.x2cpg.{Ast, Defines}
import io.joern.x2cpg.utils.IntervalKeyPool
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
    val forContext       = new ForStatementContext(stmt, new CombinedTypeSolver())
    val patternPartition = partitionPatternAstsByScope(stmt)

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
        val asts = expressions.flatMap(astsForExpression(_, ExpectedType.empty))
        scope.popBlockScope()
        asts
    }

    val patternLocals = scope.enclosingMethod.get.getAndClearUnaddedPatternLocals().map(Ast(_))

    scope.pushBlockScope()
    scope.addLocalsForPatternsToEnclosingBlock(patternPartition.patternsIntroducedToBody)
    val bodyAst = wrapInBlockWithPrefix(Nil, stmt.getBody)
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

    patternLocals :+ astWithConditionEdge
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
      iteratorAssignAstForForEach(stmt.getIterable, iteratorLocalNode, iterableType)
    val iteratorHasNextCallAst = hasNextCallAstForForEach(stmt, iteratorLocalNode)
    val variableLocal          = variableLocalForForEachBody(stmt)
    val variableAssignAst      = astForIterableForEachItemAssign(stmt, iteratorLocalNode, variableLocal)

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

  private def astForIterableForEachItemAssign(
    stmt: ForEachStmt,
    iteratorLocalNode: NewLocal,
    variableLocal: NewLocal
  ): Ast = {
    val forVariableType = variableLocal.typeFullName
    val varLocalAssignNode =
      operatorCallNode(stmt, PropertyDefaults.Code, Operators.assignment, Some(forVariableType))
    val varLocalAssignIdentifier =
      identifierNode(stmt, variableLocal.name, variableLocal.name, variableLocal.typeFullName)

    val iterNextCallSignature = composeSignature(Option(TypeConstants.Object), Option(Nil), 0)
    val iterNextCallMethodFullName =
      composeMethodFullName(TypeConstants.Iterator, NameConstants.NextCallName, iterNextCallSignature)
    val iterNextCallNode =
      callNode(
        stmt,
        code(stmt),
        NameConstants.NextCallName,
        iterNextCallMethodFullName,
        DispatchTypes.DYNAMIC_DISPATCH,
        Option(iterNextCallSignature),
        Option(TypeConstants.Object)
      )
    val iterNextCallReceiver =
      identifierNode(stmt, iteratorLocalNode.name, iteratorLocalNode.name, iteratorLocalNode.typeFullName)
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
    val idxInitializerAst = nativeForEachIdxInitializerAst(stmt, idxLocal)
    // TODO next: pass NodeTypeInfo around
    val compareAst   = nativeForEachCompareAst(stmt, iterableSource, idxLocal)
    val incrementAst = nativeForEachIncrementAst(stmt, idxLocal)
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
        logger.warn(s"Could not create AST for iterable expr $iterableExpression: $filename:l$lineNo")
        Ast()
      case iterableAstHead :: Nil => iterableAstHead
      case iterableAsts =>
        logger.warn(
          s"Found multiple ASTS for iterable expr $iterableExpression: $filename:l$lineNo\nDropping all but the first!"
        )
        iterableAsts.head
    }

    val iterableName     = nextIterableName()
    val genericSignature = binarySignatureCalculator.unspecifiedClassType
    val iterableLocalNode = localNode(
      iterableExpression,
      iterableName,
      iterableName,
      iterableType.getOrElse("ANY"),
      genericSignature = Option(genericSignature)
    )
    val iterableLocalAst = Ast(iterableLocalNode)

    val iterableAssignNode =
      operatorCallNode(iterableExpression, code(iterableExpression), Operators.assignment, iterableType)
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
    val idxName          = nextIndexName()
    val typeFullName     = TypeConstants.Int
    val genericSignature = binarySignatureCalculator.variableBinarySignature(TypeConstants.Int)
    val idxLocal =
      NewLocal()
        .name(idxName)
        .typeFullName(typeFullName)
        .code(idxName)
        .lineNumber(lineNo)
        .genericSignature(genericSignature)
    scope.enclosingBlock.get.addLocal(idxLocal, idxName)
    idxLocal
  }

  private def nativeForEachIdxInitializerAst(stmt: ForEachStmt, idxLocal: NewLocal): Ast = {
    val idxName = idxLocal.name
    val idxInitializerCallNode =
      operatorCallNode(stmt, s"int $idxName = 0", Operators.assignment, Some(TypeConstants.Int))
    val idxIdentifierArg      = identifierNode(stmt, idxName, idxName, idxLocal.typeFullName)
    val zeroLiteral           = literalNode(stmt, "0", TypeConstants.Int)
    val idxInitializerArgAsts = List(Ast(idxIdentifierArg), Ast(zeroLiteral))
    callAst(idxInitializerCallNode, idxInitializerArgAsts)
      .withRefEdge(idxIdentifierArg, idxLocal)
  }

  private def nativeForEachCompareAst(stmt: ForEachStmt, iterableSource: NodeTypeInfo, idxLocal: NewLocal): Ast = {
    val idxName = idxLocal.name

    val compareNode = operatorCallNode(
      stmt,
      code = s"$idxName < ${iterableSource.name}.${NameConstants.Length}",
      Operators.lessThan,
      typeFullName = Some(TypeConstants.Boolean)
    )
    val comparisonIdxIdentifier = identifierNode(stmt, idxName, idxName, idxLocal.typeFullName)
    val comparisonFieldAccess = operatorCallNode(
      stmt,
      code = s"${iterableSource.name}.${NameConstants.Length}",
      Operators.fieldAccess,
      typeFullName = Some(TypeConstants.Int)
    )
    val fieldAccessIdentifier =
      identifierNode(stmt, iterableSource.name, iterableSource.name, iterableSource.typeFullName.getOrElse("ANY"))
    val fieldAccessFieldIdentifier = fieldIdentifierNode(stmt, NameConstants.Length, NameConstants.Length)
    val fieldAccessArgs            = List(fieldAccessIdentifier, fieldAccessFieldIdentifier).map(Ast(_))
    val fieldAccessAst             = callAst(comparisonFieldAccess, fieldAccessArgs)
    val compareArgs                = List(Ast(comparisonIdxIdentifier), fieldAccessAst)

    // TODO: This is a workaround for a crash when looping over statically imported members. Handle those properly.
    val iterableSourceNode = localParamOrMemberFromNode(iterableSource)

    callAst(compareNode, compareArgs)
      .withRefEdge(comparisonIdxIdentifier, idxLocal)
      .withRefEdges(fieldAccessIdentifier, iterableSourceNode.toList)
  }

  private def nativeForEachIncrementAst(stmt: ForEachStmt, idxLocal: NewLocal): Ast = {
    val incrementNode =
      operatorCallNode(stmt, s"${idxLocal.name}++", Operators.postIncrement, typeFullName = Some(TypeConstants.Int))
    val incrementArg    = identifierNode(stmt, idxLocal.name, idxLocal.name, idxLocal.typeFullName)
    val incrementArgAst = Ast(incrementArg)
    callAst(incrementNode, List(incrementArgAst))
      .withRefEdge(incrementArg, idxLocal)
  }

  private def variableLocalForForEachBody(stmt: ForEachStmt): NewLocal = {
    val lineNo = line(stmt)
    // Create item local
    val maybeVariable = stmt.getVariable.getVariables.asScala.toList match {
      case Nil =>
        logger.warn(s"ForEach statement has empty variable list: $filename$lineNo")
        None
      case variable :: Nil => Some(variable)
      case variable :: _ =>
        logger.warn(s"ForEach statement defines multiple variables. Dropping all but the first: $filename$lineNo")
        Some(variable)
    }

    val genericSignature =
      maybeVariable.map(variable => binarySignatureCalculator.variableBinarySignature(variable.getType))
    val partialLocalNode = NewLocal().lineNumber(lineNo)
    genericSignature.foreach(partialLocalNode.genericSignature(_))

    maybeVariable match {
      case Some(variable) =>
        val originalName = variable.getNameAsString
        // TODO: Name mangling
        val mangledName = originalName
        val typeFullName =
          tryWithSafeStackOverflow(variable.getType).toOption.flatMap(typeInfoCalc.fullName).getOrElse("ANY")
        val localNode = partialLocalNode
          .name(mangledName)
          .code(mangledName)
          .typeFullName(typeFullName)

        scope.enclosingBlock.get.addLocal(localNode, originalName)
        localNode

      case None =>
        // Returning partialLocalNode here is fine since getting to this case means everything is broken anyways :)
        partialLocalNode
    }
  }

  private def iteratorLocalForForEach(lineNumber: Option[Int]): NewLocal = {
    val iteratorLocalName = nextIterableName()
    val genericSignature  = binarySignatureCalculator.variableBinarySignature(TypeConstants.Iterator)
    NewLocal()
      .name(iteratorLocalName)
      .code(iteratorLocalName)
      .typeFullName(TypeConstants.Iterator)
      .lineNumber(lineNumber)
      .genericSignature(genericSignature)
  }

  private def iteratorAssignAstForForEach(
    iterExpr: Expression,
    iteratorLocalNode: NewLocal,
    iterableType: Option[String]
  ): Ast = {
    val iteratorAssignNode =
      operatorCallNode(iterExpr, code(iterExpr), Operators.assignment, Some(TypeConstants.Iterator))
    val iteratorAssignIdentifier =
      identifierNode(iterExpr, iteratorLocalNode.name, iteratorLocalNode.name, iteratorLocalNode.typeFullName)

    val iteratorCallSignature = composeSignature(Option(TypeConstants.Iterator), Option(Nil), 0)
    val iteratorCallMethodName = composeMethodFullName(
      iterableType.getOrElse(Defines.UnresolvedNamespace),
      NameConstants.IteratorCallName,
      iteratorCallSignature
    )
    val iteratorCallNode =
      callNode(
        iterExpr,
        code(iterExpr),
        NameConstants.IteratorCallName,
        iteratorCallMethodName,
        DispatchTypes.DYNAMIC_DISPATCH,
        Option(iteratorCallSignature),
        Option(TypeConstants.Iterator)
      )

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

  private def hasNextCallAstForForEach(stmt: ForEachStmt, iteratorLocalNode: NewLocal): Ast = {
    val signature      = composeSignature(Option(TypeConstants.Boolean), Option(Nil), 0)
    val methodFullName = composeMethodFullName(TypeConstants.Iterator, NameConstants.HasNextCallName, signature)
    val iteratorHasNextCallNode =
      callNode(
        stmt,
        code(stmt),
        NameConstants.HasNextCallName,
        methodFullName,
        DispatchTypes.DYNAMIC_DISPATCH,
        Option(signature),
        Option(TypeConstants.Boolean)
      )
    val iteratorHasNextCallReceiver =
      identifierNode(stmt, iteratorLocalNode.name, iteratorLocalNode.name, iteratorLocalNode.typeFullName)

    callAst(iteratorHasNextCallNode, base = Some(Ast(iteratorHasNextCallReceiver)))
      .withRefEdge(iteratorHasNextCallReceiver, iteratorLocalNode)
  }

  private def variableAssignForNativeForEachBody(
    stmt: ForEachStmt,
    variableLocal: NewLocal,
    idxLocal: NewLocal,
    iterable: NodeTypeInfo
  ): Ast = {
    // Everything will be on the same line as the `for` statement, but this is the most useful
    // solution for debugging.
    val lineNo = variableLocal.lineNumber
    val varAssignNode =
      operatorCallNode(stmt, PropertyDefaults.Code, Operators.assignment, Option(variableLocal.typeFullName))

    val targetNode = identifierNode(stmt, variableLocal.name, variableLocal.name, variableLocal.typeFullName)

    val indexAccessTypeFullName = iterable.typeFullName.map(_.replaceAll(raw"\[]", ""))
    val indexAccess =
      operatorCallNode(stmt, PropertyDefaults.Code, Operators.indexAccess, indexAccessTypeFullName)

    val indexAccessIdentifier =
      identifierNode(stmt, iterable.name, iterable.name, iterable.typeFullName.getOrElse("ANY"))
    val indexAccessIndex = identifierNode(stmt, idxLocal.name, idxLocal.name, idxLocal.typeFullName)

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
    val variableAssignAst = variableAssignForNativeForEachBody(stmt, variableLocal, idxLocal, iterable)

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
