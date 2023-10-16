package io.joern.jimple2cpg.astcreation

import io.joern.jimple2cpg.astcreation.declarations.AstForDeclarationsCreator
import io.joern.jimple2cpg.astcreation.expressions.AstForExpressionsCreator
import io.joern.jimple2cpg.astcreation.statements.AstForStatementsCreator
import io.joern.x2cpg.Ast.storeInDiffGraph
import io.joern.x2cpg.datastructures.Global
import io.joern.x2cpg.utils.NodeBuilders
import io.joern.x2cpg.{Ast, AstCreatorBase, ValidationMode}
import io.shiftleft.codepropertygraph.generated.*
import io.shiftleft.codepropertygraph.generated.nodes.*
import org.objectweb.asm.Type
import org.slf4j.LoggerFactory
import overflowdb.BatchedUpdate.DiffGraphBuilder
import soot.jimple.*
import soot.tagkit.*
import soot.{Local as _, *}

import scala.collection.immutable.Seq
import scala.collection.mutable
import scala.jdk.CollectionConverters.CollectionHasAsScala
import scala.util.Try

class AstCreator(protected val filename: String, protected val cls: SootClass, global: Global)(implicit
  withSchemaValidation: ValidationMode
) extends AstCreatorBase(filename)
    with AstForDeclarationsCreator
    with AstForStatementsCreator
    with AstForExpressionsCreator {

  private val logger                                            = LoggerFactory.getLogger(getClass)
  protected val unitToAsts: mutable.HashMap[Unit, Seq[Ast]]     = mutable.HashMap.empty
  protected val controlTargets: mutable.HashMap[Seq[Ast], Unit] = mutable.HashMap.empty

  /** Add `typeName` to a global map and return it. The map is later passed to a pass that creates TYPE nodes for each
    * key in the map.
    */
  protected def registerType(typeName: String): String = {
    global.usedTypes.put(typeName, true)
    typeName
  }

  /** Entry point of AST creation. Translates a compilation unit created by JavaParser into a DiffGraph containing the
    * corresponding CPG AST.
    */
  def createAst(): DiffGraphBuilder = {
    val astRoot = astForCompilationUnit(cls)
    storeInDiffGraph(astRoot, diffGraph)
    diffGraph
  }

  /** Translate compilation unit into AST
    */
  private def astForCompilationUnit(cls: SootClass): Ast = {
    val ast = astForPackageDeclaration(cls.getPackageName)
    val namespaceBlockFullName =
      ast.root.collect { case x: NewNamespaceBlock => x.fullName }.getOrElse("none")
    ast.withChild(astForTypeDecl(cls.getType, namespaceBlockFullName))
  }

  /** Translate package declaration into AST consisting of a corresponding namespace block.
    */
  private def astForPackageDeclaration(packageDecl: String): Ast = {
    val absolutePath = new java.io.File(filename).toPath.toAbsolutePath.normalize().toString
    val name         = packageDecl.split("\\.").lastOption.getOrElse("")
    val namespaceBlock = NewNamespaceBlock()
      .name(name)
      .fullName(packageDecl)
    Ast(namespaceBlock.filename(absolutePath).order(1))
  }

  protected def getEvaluationStrategy(typ: soot.Type): String =
    typ match {
      case _: PrimType    => EvaluationStrategies.BY_VALUE
      case _: VoidType    => EvaluationStrategies.BY_VALUE
      case _: NullType    => EvaluationStrategies.BY_VALUE
      case _: RefLikeType => EvaluationStrategies.BY_REFERENCE
      case _              => EvaluationStrategies.BY_SHARING
    }

  protected def isIgnoredUnit(unit: soot.Unit): Boolean = {
    unit match {
      case _: IdentityStmt => true
      case _: NopStmt      => true
      case _               => false
    }
  }

  protected def astsForValue(value: soot.Value, order: Int, parentUnit: soot.Unit): Seq[Ast] = {
    value match {
      case x: Expr               => astsForExpression(x, order, parentUnit)
      case x: soot.Local         => Seq(astForLocal(x, order, parentUnit))
      case x: CaughtExceptionRef => Seq(astForCaughtExceptionRef(x, order, parentUnit))
      case x: Constant           => Seq(astForConstantExpr(x, order))
      case x: FieldRef           => Seq(astForFieldRef(x, order, parentUnit))
      case x: ThisRef            => Seq(createThisNode(x))
      case x: ParameterRef       => Seq(createParameterNode(x, order))
      case x: IdentityRef        => Seq(astForIdentityRef(x, order, parentUnit))
      case x: ArrayRef           => Seq(astForArrayRef(x, order, parentUnit))
      case x =>
        logger.warn(s"Unhandled soot.Value type ${x.getClass}")
        Seq()
    }
  }

  protected def astForArrayRef(arrRef: ArrayRef, order: Int, parentUnit: soot.Unit): Ast = {
    val indexAccess = NewCall()
      .name(Operators.indexAccess)
      .methodFullName(Operators.indexAccess)
      .dispatchType(DispatchTypes.STATIC_DISPATCH)
      .code(arrRef.toString())
      .order(order)
      .argumentIndex(order)
      .lineNumber(line(parentUnit))
      .columnNumber(column(parentUnit))
      .typeFullName(registerType(arrRef.getType.toQuotedString))

    val astChildren = astsForValue(arrRef.getBase, 1, parentUnit) ++ astsForValue(arrRef.getIndex, 2, parentUnit)
    Ast(indexAccess)
      .withChildren(astChildren)
      .withArgEdges(indexAccess, astChildren.flatMap(_.root))
  }

  protected def astForLocal(local: soot.Local, order: Int, parentUnit: soot.Unit): Ast = {
    val name         = local.getName
    val typeFullName = registerType(local.getType.toQuotedString)
    Ast(
      NewIdentifier()
        .name(name)
        .lineNumber(line(parentUnit))
        .columnNumber(column(parentUnit))
        .order(order)
        .argumentIndex(order)
        .code(name)
        .typeFullName(typeFullName)
    )
  }

  protected def astForIdentityRef(x: IdentityRef, order: Int, parentUnit: soot.Unit): Ast = {
    Ast(
      NewIdentifier()
        .code(x.toString())
        .name(x.toString())
        .order(order)
        .argumentIndex(order)
        .typeFullName(registerType(x.getType.toQuotedString))
        .lineNumber(line(parentUnit))
        .columnNumber(column(parentUnit))
    )
  }

  protected def createThisNode(method: ThisRef): Ast = {
    Ast(
      NewIdentifier()
        .name("this")
        .code("this")
        .typeFullName(registerType(method.getType.toQuotedString))
        .dynamicTypeHintFullName(Seq(registerType(method.getType.toQuotedString)))
        .order(0)
        .argumentIndex(0)
    )
  }

  protected def createThisNode(method: SootMethod, builder: NewNode): Ast = createThisNode(method.makeRef(), builder)

  protected def createThisNode(method: SootMethodRef, builder: NewNode): Ast = {
    if (!method.isStatic || method.isConstructor) {
      val parentType = registerType(Try(method.getDeclaringClass.getType.toQuotedString).getOrElse("ANY"))
      Ast(builder match {
        case x: NewIdentifier =>
          x.name("this")
            .code("this")
            .typeFullName(parentType)
            .order(0)
            .argumentIndex(0)
            .dynamicTypeHintFullName(Seq(parentType))
        case _: NewMethodParameterIn =>
          NodeBuilders.newThisParameterNode(
            typeFullName = parentType,
            dynamicTypeHintFullName = Seq(parentType),
            line = line(Try(method.tryResolve()).getOrElse(null))
          )
        case x => x
      })
    } else {
      Ast()
    }
  }

  protected def astForFieldRef(fieldRef: FieldRef, order: Int, parentUnit: soot.Unit): Ast = {
    val leftOpString = fieldRef match {
      case x: StaticFieldRef   => x.getFieldRef.declaringClass().toString
      case x: InstanceFieldRef => x.getBase.toString()
      case _                   => fieldRef.getFieldRef.declaringClass().toString
    }
    val leftOpType = fieldRef match {
      case x: StaticFieldRef   => x.getFieldRef.declaringClass().getType
      case x: InstanceFieldRef => x.getBase.getType
      case _                   => fieldRef.getFieldRef.declaringClass().getType
    }

    val fieldAccessBlock = NewCall()
      .name(Operators.fieldAccess)
      .code(s"$leftOpString.${fieldRef.getFieldRef.name()}")
      .typeFullName(registerType(fieldRef.getType.toQuotedString))
      .methodFullName(Operators.fieldAccess)
      .dispatchType(DispatchTypes.STATIC_DISPATCH)
      .order(order)
      .argumentIndex(order)
      .lineNumber(line(parentUnit))
      .columnNumber(column(parentUnit))

    val argAsts = Seq(
      NewIdentifier()
        .order(1)
        .argumentIndex(1)
        .lineNumber(line(parentUnit))
        .columnNumber(column(parentUnit))
        .name(leftOpString)
        .code(leftOpString)
        .typeFullName(registerType(leftOpType.toQuotedString)),
      NewFieldIdentifier()
        .order(2)
        .argumentIndex(2)
        .lineNumber(line(parentUnit))
        .columnNumber(column(parentUnit))
        .canonicalName(fieldRef.getFieldRef.name())
        .code(fieldRef.getFieldRef.name())
    ).map(Ast(_))

    Ast(fieldAccessBlock)
      .withChildren(argAsts)
      .withArgEdges(fieldAccessBlock, argAsts.flatMap(_.root))
  }

  private def astForCaughtExceptionRef(caughtException: CaughtExceptionRef, order: Int, parentUnit: soot.Unit): Ast = {
    Ast(
      NewIdentifier()
        .order(order)
        .argumentIndex(order)
        .lineNumber(line(parentUnit))
        .columnNumber(column(parentUnit))
        .name(caughtException.toString())
        .code(caughtException.toString())
        .typeFullName(registerType(caughtException.getType.toQuotedString))
    )
  }

  private def astForConstantExpr(constant: Constant, order: Int): Ast = {
    constant match {
      case x: ClassConstant =>
        Ast(
          NewLiteral()
            .order(order)
            .argumentIndex(order)
            .code(s"${x.value.parseAsJavaType}.class")
            .typeFullName(registerType(x.getType.toQuotedString))
        )
      case _: NullConstant =>
        Ast(
          NewLiteral()
            .order(order)
            .argumentIndex(order)
            .code("null")
            .typeFullName(registerType("null"))
        )
      case _ =>
        Ast(
          NewLiteral()
            .order(order)
            .argumentIndex(order)
            .code(constant.toString)
            .typeFullName(registerType(constant.getType.toQuotedString))
        )
    }
  }

  private def callAst(rootNode: NewNode, args: Seq[Ast]): Ast = {
    Ast(rootNode)
      .withChildren(args)
      .withArgEdges(rootNode, args.flatMap(_.root))
  }

  def line(node: Host): Option[Integer] = {
    if (node == null) None
    else if (node.getJavaSourceStartLineNumber == -1) None
    else Option(node.getJavaSourceStartLineNumber)
  }

  def column(node: Host): Option[Integer] = {
    if (node == null) None
    else if (node.getJavaSourceStartColumnNumber == -1) None
    else Option(node.getJavaSourceStartColumnNumber)
  }

  def withOrder[T <: Any, X](nodeList: java.util.List[T])(f: (T, Int) => X): Seq[X] = {
    nodeList.asScala.zipWithIndex.map { case (x, i) =>
      f(x, i + 1)
    }.toSeq
  }

  def withOrder[T <: Any, X](nodeList: Iterable[T])(f: (T, Int) => X): Seq[X] = {
    nodeList.zipWithIndex.map { case (x, i) =>
      f(x, i + 1)
    }.toSeq
  }

}

/** String extensions for strings describing JVM operators.
  */
implicit class JvmStringOpts(s: String) {

  /** Parses the string as a ASM Java type descriptor and returns a fully qualified type. Also converts symbols such as
    * <code>I</code> to <code>int</code>.
    * @return
    */
  def parseAsJavaType: String = Type.getType(s).getClassName.replaceAll("/", ".")

}
