package io.joern.javasrc2cpg.astcreation.expressions

import com.github.javaparser.ast.Node
import com.github.javaparser.ast.expr.{PatternExpr, RecordPatternExpr, TypePatternExpr}
import io.joern.javasrc2cpg.astcreation.AstCreator
import io.joern.javasrc2cpg.jartypereader.model.Model.TypeConstants
import io.joern.javasrc2cpg.scope.Scope.NewVariableNode
import io.joern.javasrc2cpg.util.Util
import io.joern.x2cpg.{Ast, Defines}
import io.joern.x2cpg.utils.AstPropertiesUtil.*
import io.shiftleft.codepropertygraph.generated.nodes.{AstNodeNew, NewCall, NewIdentifier}
import io.joern.x2cpg.utils.NodeBuilders.*
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, Operators}

import scala.jdk.CollectionConverters.*
import scala.jdk.OptionConverters.RichOptional

trait AstForPatternExpressionsCreator { this: AstCreator =>

  /** In the lowering for instanceof expressions with patterns like `X instanceof Foo f`, the first argument to
    * `instanceof` (in this case `X`) appears in the CPG at least 2 times:
    *   - once for the `X instanceof Foo` check
    *   - once for the `Foo f = (Foo) X` assignment.
    *
    * If X is an identifier or field access, then this is fine. If X is a call which could have side-effects, however,
    * then this representation could lead to incorrect behaviour.
    *
    * This method solves this problem by taking the CPG lowering for X as input and returning a tuple (initializerAst,
    * referenceAst) where initializerAst should be used as the LHS arg for the first instanceof call, while referenceAst
    * should be used for any future references to X (for example in the variable assignment or nested instanceof calls
    * for records).
    *
    * If X is an identifier or field access, then initializerAst is simply the input and referenceAst is a copy of it.
    *
    * If X is not an identifier or field access, then a temporary variable `$objN` is created. initializerAst is then an
    * assignment AST `$objN = X` and referenceAst is the identifier ast `$objN`.
    */
  private[astcreation] def initializerAndReferenceAstsForPatternInitializer(
    rootNode: Node,
    patternInitAst: Ast
  ): (Ast, Ast) = {
    patternInitAst.root match {
      case Some(identifier: NewIdentifier) =>
        (patternInitAst, patternInitAst.subTreeCopy(identifier))

      case Some(fieldAccess: NewCall) if fieldAccess.name == Operators.fieldAccess =>
        (patternInitAst, patternInitAst.subTreeCopy(patternInitAst.root.get.asInstanceOf[AstNodeNew]))

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

        val initAst =
          callAst(tmpAssignmentNode, Ast(tmpIdentifier) :: patternInitAst :: Nil).withRefEdge(tmpIdentifier, tmpLocal)

        val tmpIdentifierCopy = tmpIdentifier.copy
        val referenceAst      = Ast(tmpIdentifierCopy).withRefEdge(tmpIdentifierCopy, tmpLocal)

        (initAst, referenceAst)
    }
  }

  private def castAstIfNecessary(patternExpr: PatternExpr, patternType: String, initializerAst: Ast): Ast = {
    val initializerType = initializerAst.rootType
    if (isResolvedTypeFullName(patternType) && initializerType.contains(patternType)) {
      initializerAst
    } else {
      val castType = typeRefNode(patternExpr, code(patternExpr.getType), patternType)
      val castNode =
        newOperatorCallNode(
          Operators.cast,
          s"(${castType.code}) ${initializerAst.rootCodeOrEmpty}",
          Option(patternType)
        )
      callAst(castNode, Ast(castType) :: initializerAst :: Nil)
    }
  }

  private def createAndPushAssignmentForTypePattern(
    typePatternExpr: TypePatternExpr,
    parentInitializerAst: Ast
  ): Unit = {
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
    val variableTypeCode  = tryWithSafeStackOverflow(code(typePatternExpr.getType)).getOrElse(variableType)
    val patternLocal      = localNode(typePatternExpr, variableName, code(typePatternExpr), variableType)
    val patternIdentifier = identifierNode(typePatternExpr, variableName, variableName, variableType)

    val initializerAst = castAstIfNecessary(typePatternExpr, variableType, parentInitializerAst)

    val initializerAssignmentCall = newOperatorCallNode(
      Operators.assignment,
      s"$variableName = ${initializerAst.rootCodeOrEmpty}",
      Option(variableType),
      line(typePatternExpr),
      column(typePatternExpr)
    )
    val initializerAssignmentAst = callAst(initializerAssignmentCall, Ast(patternIdentifier) :: initializerAst :: Nil)
      .withRefEdge(patternIdentifier, patternLocal)

    scope.enclosingMethod.foreach { methodScope =>
      methodScope.putPatternVariableInfo(typePatternExpr, patternLocal, initializerAssignmentAst)
    }
  }

  private def accessorAstsForPatternList(
    recordPatternExpr: RecordPatternExpr,
    recordTypeFullName: String,
    accessorReceiverAst: Ast
  ): List[(PatternExpr, Ast)] = {
    val resolvedRecordType = tryWithSafeStackOverflow(recordPatternExpr.getType().resolve().asReferenceType()).toOption

    val patternList = recordPatternExpr.getPatternList.asScala.toList
    val fieldNames = resolvedRecordType
      .flatMap(_.getTypeDeclaration.toScala)
      .map(_.getDeclaredFields.asScala.map(_.getName).toList)
      .getOrElse(patternList.map(_ => Defines.UnknownField))

    patternList.zip(fieldNames).zipWithIndex.map { case ((patternExpr, fieldName), idx) =>
      val patternTypeFullName = tryWithSafeStackOverflow(patternExpr.getType).toOption
        .map { typ =>
          scope
            .lookupScopeType(typ.asString())
            .map(_.typeFullName)
            .orElse(typeInfoCalc.fullName(typ))
            .getOrElse(defaultTypeFallback(typ))
        }
        .getOrElse(defaultTypeFallback())

      val fieldTypeFullName = resolvedRecordType
        .flatMap(_.getTypeDeclaration.toScala)
        .flatMap(typeDecl => tryWithSafeStackOverflow(typeDecl.getField(fieldName).getType).toOption)
        .flatMap(typeInfoCalc.fullName)

      val signature = composeSignature(fieldTypeFullName, Option(Nil), 0)
      val typeDeclFullName =
        if (isResolvedTypeFullName(recordTypeFullName))
          recordTypeFullName
        else
          s"${Defines.UnresolvedNamespace}.${code(recordPatternExpr.getType)}"
      val methodFullName = Util.composeMethodFullName(typeDeclFullName, fieldName, signature)
      val methodCodePrefix = accessorReceiverAst.root match {
        case Some(call: NewCall) if call.name.startsWith("<operator") => s"(${call.code})"
        case Some(root: AstNodeNew)                                   => root.code
        case _                                                        => ""

      }
      val methodCode = s"$methodCodePrefix.$fieldName()"

      val fieldAccessorCall = callNode(
        patternExpr,
        methodCode,
        fieldName,
        methodFullName,
        DispatchTypes.DYNAMIC_DISPATCH,
        Option(signature),
        fieldTypeFullName.orElse(Option(defaultTypeFallback()))
      )

      val lhsAst =
        if (idx == 0)
          accessorReceiverAst
        else
          accessorReceiverAst.subTreeCopy(accessorReceiverAst.root.get.asInstanceOf[AstNodeNew])

      val fieldAccessorAst = callAst(fieldAccessorCall, lhsAst :: Nil)

      (patternExpr, fieldAccessorAst)
    }

  }

  private[astcreation] def typeCheckAstForPattern(patternExpr: PatternExpr, lhsAst: Ast): Ast = {
    val patternTypeFullName = {
      tryWithSafeStackOverflow(patternExpr.getType).toOption
        .map(typ =>
          scope
            .lookupScopeType(typ.asString())
            .map(_.typeFullName)
            .orElse(typeInfoCalc.fullName(typ))
            .getOrElse(defaultTypeFallback(typ))
        )
    }.getOrElse(defaultTypeFallback())

    val patternTypeRef = typeRefNode(patternExpr.getType, code(patternExpr.getType), patternTypeFullName)

    patternExpr match {
      case typePatternExpr: TypePatternExpr =>
        val (initializerAst, referenceAst) = initializerAndReferenceAstsForPatternInitializer(typePatternExpr, lhsAst)

        val lhsCode = initializerAst.root match {
          case Some(identifier: NewIdentifier)                           => identifier.code
          case Some(call: NewCall) if call.name == Operators.fieldAccess => call.code
          case Some(astNodeNew: AstNodeNew)                              => s"(${astNodeNew.code})"
          case _                                                         => ""
        }
        val instanceOfCall = newOperatorCallNode(
          Operators.instanceOf,
          s"$lhsCode instanceof ${code(patternExpr.getType)}",
          Option(TypeConstants.Boolean)
        )

        createAndPushAssignmentForTypePattern(typePatternExpr, referenceAst)

        callAst(instanceOfCall, initializerAst :: Ast(patternTypeRef) :: Nil)

      case recordPatternExpr: RecordPatternExpr =>
        val (initializerAst, referenceAst) = initializerAndReferenceAstsForPatternInitializer(patternExpr, lhsAst)
        val lhsCode = initializerAst.root match {
          case Some(identifier: NewIdentifier)                           => identifier.code
          case Some(call: NewCall) if call.name == Operators.fieldAccess => call.code
          case Some(astNode: AstNodeNew)                                 => s"(${astNode.code})"
          case _                                                         => ""
        }
        val instanceOfCall = newOperatorCallNode(
          Operators.instanceOf,
          s"$lhsCode instanceof ${code(patternExpr.getType)}",
          Option(TypeConstants.Boolean)
        )
        val topLevelInstanceOfAst = callAst(instanceOfCall, initializerAst :: Ast(patternTypeRef) :: Nil)

        val recordTypeFullName = tryWithSafeStackOverflow(recordPatternExpr.getType)
          .map(typ =>
            scope.lookupType(code(typ)).orElse(typeInfoCalc.fullName(typ)).getOrElse(defaultTypeFallback(typ))
          )
          .getOrElse(defaultTypeFallback())

        val recordLhsAst = castAstIfNecessary(
          recordPatternExpr,
          recordTypeFullName,
          referenceAst.subTreeCopy(referenceAst.root.get.asInstanceOf[AstNodeNew])
        )

        val accessorAsts = accessorAstsForPatternList(recordPatternExpr, recordTypeFullName, recordLhsAst)

        val accessorAstsWithInit = ((recordPatternExpr, topLevelInstanceOfAst) :: accessorAsts).reverse

        val accumulator = typeCheckAstForPattern(accessorAstsWithInit.head._1, accessorAstsWithInit.head._2)

        accessorAstsWithInit.tail.foldLeft(accumulator) { case (accumulatorAst, (childPattern, childFieldAccessor)) =>
          val astToAdd =
            if (childPattern.eq(recordPatternExpr))
              childFieldAccessor
            else
              typeCheckAstForPattern(childPattern, childFieldAccessor)

          val andNode = newOperatorCallNode(
            Operators.logicalAnd,
            s"(${astToAdd.rootCodeOrEmpty}) && (${accumulatorAst.rootCodeOrEmpty})",
            Option(TypeConstants.Boolean),
            line(childPattern),
            column(childPattern)
          )

          callAst(andNode, astToAdd :: accumulatorAst :: Nil)
        }
    }
  }
}
