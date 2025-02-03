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
import org.slf4j.LoggerFactory

import java.util
import scala.collection.mutable
import scala.jdk.CollectionConverters.*
import scala.jdk.OptionConverters.RichOptional

class PatternInitAndRefAsts(private val initAst: Ast, private val refAst: Ast) {
  private var getCount: Int = -1
  def get: Ast = {
    getCount += 1
    getCount match {
      case 0 => initAst
      case 1 => refAst
      case _ => refAst.subTreeCopy(refAst.root.get.asInstanceOf[AstNodeNew])
    }
  }

  def rootType: Option[String] = initAst.rootType

  def asTuple: (Ast, Ast) = (initAst, refAst)
}

object PatternInitAndRefAsts {
  def apply(initAst: Ast, refAst: Ast): PatternInitAndRefAsts = new PatternInitAndRefAsts(initAst, refAst)

  def apply(initAst: Ast): PatternInitAndRefAsts =
    new PatternInitAndRefAsts(initAst, initAst.subTreeCopy(initAst.root.get.asInstanceOf[AstNodeNew]))
}

trait AstForPatternExpressionsCreator { this: AstCreator =>

  private val logger = LoggerFactory.getLogger(this.getClass)

  trait PatternInitTreeNode(val patternExpr: PatternExpr) {
    def getAst: Ast

    def typeFullName: Option[String]
  }

  class PatternInitRoot(patternExpr: PatternExpr, ast: PatternInitAndRefAsts) extends PatternInitTreeNode(patternExpr) {
    override def getAst: Ast = ast.get

    override def typeFullName: Option[String] = ast.rootType
  }

  class PatternInitNode(
    parentNode: PatternInitTreeNode,
    patternExpr: PatternExpr,
    fieldName: String,
    fieldTypeFullName: Option[String],
    requiresTemporaryVariable: Boolean
  ) extends PatternInitTreeNode(patternExpr) {
    private var cachedResult: Option[PatternInitAndRefAsts] = None

    override def typeFullName: Option[String] = fieldTypeFullName

    override def getAst: Ast = {
      cachedResult.map(_.get).getOrElse {
        val parentAst = parentNode.getAst
        val patternTypeFullName = tryWithSafeStackOverflow(patternExpr.getType).toOption
          .map { typ =>
            scope
              .lookupScopeType(typ.asString())
              .map(_.typeFullName)
              .orElse(typeInfoCalc.fullName(typ))
              .getOrElse(defaultTypeFallback(typ))
          }
          .getOrElse(defaultTypeFallback())

        val parentPatternType = getPatternTypeFullName(parentNode.patternExpr)
        val lhsAst            = castAstIfNecessary(parentNode.patternExpr, parentPatternType, parentAst)

        val signature = composeSignature(fieldTypeFullName, Option(Nil), 0)
        val typeDeclFullName =
          if (isResolvedTypeFullName(parentPatternType))
            parentPatternType
          else
            s"${Defines.UnresolvedNamespace}.${code(parentNode.patternExpr.getType)}"
        val methodFullName = Util.composeMethodFullName(typeDeclFullName, fieldName, signature)
        val methodCodePrefix = lhsAst.root match {
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

        val fieldAccessorAst = callAst(fieldAccessorCall, lhsAst :: Nil)

        val patternInitWithRef = if (requiresTemporaryVariable) {
          val patternInitWithRef = initAndRefAstsForPatternInitializer(patternExpr, fieldAccessorAst)
          patternInitWithRef
        } else {
          PatternInitAndRefAsts(fieldAccessorAst)
        }

        cachedResult = Option(patternInitWithRef)

        patternInitWithRef.get
      }
    }
  }

  /** In the lowering for instanceof expressions with patterns like `X instanceof Foo f`, the first argument to
    * `instanceof` (in this case `X`) appears in the CPG at least 2 times:
    *   - once for the `X instanceof Foo` check
    *   - once for the `Foo f = (Foo) X` assignment.
    *
    * If X is an identifier or field access, then this is fine. If X is a call which could have side-effects, however,
    * then this representation could lead to incorrect behaviour.
    *
    * This method solves this problem by taking the CPG lowering for X as input and returning a PatternInitAndRefAsts
    * object. The first time `get` is called on one of these, the init AST is return. Every future get call returns the
    * reference AST, ensuring that the variable is initialized exactly once
    */
  private[astcreation] def initAndRefAstsForPatternInitializer(
    rootNode: Node,
    patternInitAst: Ast
  ): PatternInitAndRefAsts = {
    patternInitAst.root match {
      case Some(identifier: NewIdentifier) =>
        PatternInitAndRefAsts(patternInitAst, patternInitAst.subTreeCopy(identifier))

      case Some(fieldAccess: NewCall) if fieldAccess.name == Operators.fieldAccess =>
        PatternInitAndRefAsts(
          patternInitAst,
          patternInitAst.subTreeCopy(patternInitAst.root.get.asInstanceOf[AstNodeNew])
        )

      case _ =>
        val tmpName = tempNameProvider.next
        val tmpType = patternInitAst.rootType.getOrElse(TypeConstants.Object)
        val tmpLocal = localNode(
          rootNode,
          tmpName,
          tmpName,
          tmpType,
          genericSignature = Option(binarySignatureCalculator.unspecifiedClassType)
        )
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

        PatternInitAndRefAsts(initAst, referenceAst)
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
          Option(patternType),
          line(patternExpr),
          column(patternExpr)
        )
      callAst(castNode, Ast(castType) :: initializerAst :: Nil)
    }
  }

  private def createAndPushAssignmentForTypePattern(patternNode: PatternInitTreeNode): Unit = {
    patternNode.patternExpr match {
      case recordPatternExpr: RecordPatternExpr =>
        logger.warn(s"Attempting to create assignment for record pattern expr ${code(recordPatternExpr)}")

      case typePatternExpr: TypePatternExpr =>
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
        val genericSignature = binarySignatureCalculator.variableBinarySignature(typePatternExpr.getType)
        val patternLocal = localNode(
          typePatternExpr,
          variableName,
          code(typePatternExpr),
          variableType,
          genericSignature = Option(genericSignature)
        )
        val patternIdentifier = identifierNode(typePatternExpr, variableName, variableName, variableType)

        val initializerAst = castAstIfNecessary(typePatternExpr, variableType, patternNode.getAst)

        val initializerAssignmentCall = newOperatorCallNode(
          Operators.assignment,
          s"$variableName = ${initializerAst.rootCodeOrEmpty}",
          Option(variableType),
          line(typePatternExpr),
          column(typePatternExpr)
        )
        val initializerAssignmentAst =
          callAst(initializerAssignmentCall, Ast(patternIdentifier) :: initializerAst :: Nil)
            .withRefEdge(patternIdentifier, patternLocal)

        scope.enclosingMethod.foreach { methodScope =>
          methodScope.putPatternVariableInfo(typePatternExpr, patternLocal, initializerAssignmentAst)
        }

    }
  }

  private[astcreation] def instanceOfAstForPattern(patternExpr: PatternExpr, lhsAst: Ast): Ast = {
    val patternTreeNode = PatternInitRoot(patternExpr, initAndRefAstsForPatternInitializer(patternExpr, lhsAst))
    val typePatternBuffer: mutable.ListBuffer[PatternInitTreeNode] = mutable.ListBuffer()
    if (patternExpr.isTypePatternExpr) {
      typePatternBuffer.append(patternTreeNode)
    }
    val typeCheckAst = typeCheckAstForPattern(patternExpr, patternTreeNode, typePatternBuffer).get

    typePatternBuffer.foreach(createAndPushAssignmentForTypePattern)
    typeCheckAst
  }

  private def typeCheckAstForPattern(
    patternExpr: PatternExpr,
    parentInitNode: PatternInitTreeNode,
    typePatternBuffer: mutable.ListBuffer[PatternInitTreeNode]
  ): Option[Ast] = {
    val patternTypeFullName = getPatternTypeFullName(patternExpr)

    val isInstanceOfRequired =
      parentInitNode.isInstanceOf[PatternInitRoot]
        || !isResolvedTypeFullName(patternTypeFullName)
        || !parentInitNode.typeFullName.contains(patternTypeFullName)

    val instanceOfAst =
      Option.when(isInstanceOfRequired)(buildInstanceOfAst(patternExpr, parentInitNode, patternTypeFullName))

    patternExpr match {
      case typePatternExpr: TypePatternExpr =>
        instanceOfAst

      case recordPatternExpr: RecordPatternExpr =>
        val fieldAccessorInits =
          initNodesForRecordFieldAccessors(recordPatternExpr, patternTypeFullName, parentInitNode)

        val fieldInstanceOfAsts = fieldAccessorInits.flatMap { fieldInitNode =>
          if (fieldInitNode.patternExpr.isTypePatternExpr) {
            typePatternBuffer.append(fieldInitNode)
          }
          typeCheckAstForPattern(fieldInitNode.patternExpr, fieldInitNode, typePatternBuffer).map { ast =>
            (fieldInitNode.patternExpr, ast)
          }
        }

        (instanceOfAst.map(ast => (recordPatternExpr, ast)).toList ++ fieldInstanceOfAsts).reverse match {
          case Nil => None

          case accumulator :: rest =>
            val result = rest.foldLeft(accumulator._2) { case (accumulatorAst, (childPattern, astToAdd)) =>
              val andNode = newOperatorCallNode(
                Operators.logicalAnd,
                s"(${astToAdd.rootCodeOrEmpty}) && (${accumulatorAst.rootCodeOrEmpty})",
                Option(TypeConstants.Boolean),
                line(childPattern),
                column(childPattern)
              )

              callAst(andNode, astToAdd :: accumulatorAst :: Nil)
            }
            Option(result)
        }
    }
  }

  private def initNodesForRecordFieldAccessors(
    recordPatternExpr: RecordPatternExpr,
    recordTypeFullName: String,
    parentInitNode: PatternInitTreeNode
  ): List[PatternInitNode] = {
    val resolvedRecordType = tryWithSafeStackOverflow(recordPatternExpr.getType().resolve().asReferenceType()).toOption

    val patternList = recordPatternExpr.getPatternList.asScala.toList
    val fieldNames = resolvedRecordType
      .flatMap(_.getTypeDeclaration.toScala)
      .map(_.getDeclaredFields.asScala.map(_.getName).toList)
      .getOrElse(patternList.map(_ => Defines.UnknownField))

    patternList.zip(fieldNames).map { case (childPatternExpr, fieldName) =>
      val childTypeFullName = getPatternTypeFullName(childPatternExpr) match {
        case typeFullName if isResolvedTypeFullName(typeFullName) => Option(typeFullName)
        case _                                                    => None
      }

      val fieldTypeFullName = resolvedRecordType
        .flatMap(_.getTypeDeclaration.toScala)
        .flatMap(typeDecl => tryWithSafeStackOverflow(typeDecl.getField(fieldName).getType).toOption)
        .flatMap(typeInfoCalc.fullName)

      val childIsBranchingNode =
        childPatternExpr.isRecordPatternExpr && childPatternExpr.asRecordPatternExpr().getPatternList.size() > 1
      val childTypeIsResolved = childTypeFullName.exists(isResolvedTypeFullName)
      val requiresTemporaryVariable =
        childIsBranchingNode || !childTypeIsResolved || childTypeFullName != fieldTypeFullName

      PatternInitNode(parentInitNode, childPatternExpr, fieldName, fieldTypeFullName, requiresTemporaryVariable)
    }
  }

  private def buildInstanceOfAst(
    patternExpr: PatternExpr,
    parentInitNode: PatternInitTreeNode,
    patternTypeFullName: String
  ): Ast = {
    val patternTypeRef = typeRefNode(patternExpr.getType, code(patternExpr.getType), patternTypeFullName)
    val initializerAst = parentInitNode.getAst

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
    callAst(instanceOfCall, initializerAst :: Ast(patternTypeRef) :: Nil)
  }

  private def getPatternTypeFullName(patternExpr: PatternExpr): String = {
    tryWithSafeStackOverflow(patternExpr.getType).toOption
      .map(typ =>
        scope
          .lookupScopeType(typ.asString())
          .map(_.typeFullName)
          .orElse(typeInfoCalc.fullName(typ))
          .getOrElse(defaultTypeFallback(typ))
      )
      .getOrElse(defaultTypeFallback())
  }
}
