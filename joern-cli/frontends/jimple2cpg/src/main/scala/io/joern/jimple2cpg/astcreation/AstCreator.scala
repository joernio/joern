package io.joern.jimple2cpg.astcreation

import io.joern.jimple2cpg.astcreation.declarations.AstForDeclarationsCreator
import io.joern.jimple2cpg.astcreation.expressions.AstForExpressionsCreator
import io.joern.jimple2cpg.astcreation.statements.AstForStatementsCreator
import io.joern.x2cpg.Ast.storeInDiffGraph
import io.joern.x2cpg.datastructures.Global
import io.joern.x2cpg.utils.NodeBuilders
import io.joern.x2cpg.*
import io.shiftleft.codepropertygraph.generated.*
import io.shiftleft.codepropertygraph.generated.nodes.*
import org.objectweb.asm.Type
import org.slf4j.LoggerFactory
import overflowdb.BatchedUpdate.DiffGraphBuilder
import soot.jimple.*
import soot.tagkit.*
import soot.{Unit as SUnit, Local as _, *}

import scala.collection.immutable.Seq
import scala.collection.mutable
import scala.util.Try

class AstCreator(
  protected val filename: String,
  protected val cls: SootClass,
  global: Global,
  fileContent: Option[String] = None
)(implicit withSchemaValidation: ValidationMode)
    extends AstCreatorBase(filename)
    with AstForDeclarationsCreator
    with AstForStatementsCreator
    with AstForExpressionsCreator
    with AstNodeBuilder[Host, AstCreator] {

  private val logger = LoggerFactory.getLogger(getClass)

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

    if (fileContent.isDefined) {
      val fileNode = NewFile().name(filename).order(0)
      fileContent.foreach(fileNode.content(_))
      diffGraph.addNode(fileNode)
    }

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
    Ast(namespaceBlock.filename(absolutePath))
  }

  protected def getEvaluationStrategy(typ: soot.Type): String =
    typ match {
      case _: PrimType => EvaluationStrategies.BY_VALUE
      case _: VoidType => EvaluationStrategies.BY_VALUE
      case _: NullType => EvaluationStrategies.BY_VALUE
      case _           => EvaluationStrategies.BY_SHARING
    }

  protected def isIgnoredUnit(unit: SUnit): Boolean = {
    unit match {
      case x: IdentityStmt if x.getRightOp.isInstanceOf[CaughtExceptionRef] => false
      case _: IdentityStmt                                                  => true
      case _: NopStmt                                                       => true
      case _                                                                => false
    }
  }

  protected def astsForValue(value: soot.Value, parentUnit: SUnit): Seq[Ast] = {
    value match {
      case x: Expr               => astsForExpression(x, parentUnit)
      case x: soot.Local         => Seq(astForLocal(x, parentUnit))
      case x: CaughtExceptionRef => Seq(astForCaughtExceptionRef(x, parentUnit))
      case x: Constant           => Seq(astForConstantExpr(x))
      case x: FieldRef           => Seq(astForFieldRef(x, parentUnit))
      case x: ThisRef            => Seq(createThisNode(x))
      case x: ParameterRef       => Seq(astForParameterRef(x, parentUnit))
      case x: IdentityRef        => Seq(astForIdentityRef(x, parentUnit))
      case x: ArrayRef           => Seq(astForArrayRef(x, parentUnit))
      case x =>
        logger.warn(s"Unhandled soot.Value type ${x.getClass}")
        Seq()
    }
  }

  protected def astForArrayRef(arrRef: ArrayRef, parentUnit: SUnit): Ast = {
    val indexAccess = NewCall()
      .name(Operators.indexAccess)
      .methodFullName(Operators.indexAccess)
      .dispatchType(DispatchTypes.STATIC_DISPATCH)
      .code(arrRef.toString())
      .lineNumber(line(parentUnit))
      .columnNumber(column(parentUnit))
      .typeFullName(registerType(arrRef.getType.toQuotedString))

    val astChildren = astsForValue(arrRef.getBase, parentUnit) ++ astsForValue(arrRef.getIndex, parentUnit)
    Ast(indexAccess)
      .withChildren(astChildren)
      .withArgEdges(indexAccess, astChildren.flatMap(_.root), 1)
  }

  protected def astForLocal(local: soot.Local, parentUnit: SUnit): Ast = {
    val name         = local.getName
    val typeFullName = registerType(local.getType.toQuotedString)
    Ast(
      NewIdentifier()
        .name(name)
        .lineNumber(line(parentUnit))
        .columnNumber(column(parentUnit))
        .code(name)
        .typeFullName(typeFullName)
    )
  }

  protected def astForIdentityRef(x: IdentityRef, parentUnit: SUnit): Ast = {
    Ast(
      NewIdentifier()
        .code(x.toString())
        .name(x.toString())
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

  protected def astForFieldRef(fieldRef: FieldRef, parentUnit: SUnit): Ast = {
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
    // TODO
    Ast(fieldAccessBlock)
      .withChildren(argAsts)
      .withArgEdges(fieldAccessBlock, argAsts.flatMap(_.root), 1)
  }

  private def astForCaughtExceptionRef(caughtException: CaughtExceptionRef, parentUnit: SUnit): Ast = {
    Ast(
      NewIdentifier()
        .lineNumber(line(parentUnit))
        .columnNumber(column(parentUnit))
        .name(caughtException.toString())
        .code(caughtException.toString())
        .typeFullName(registerType(caughtException.getType.toQuotedString))
    )
  }

  private def astForConstantExpr(constant: Constant): Ast = {
    constant match {
      case x: ClassConstant =>
        Ast(
          NewLiteral()
            .code(s"${x.value.parseAsJavaType}.class")
            .typeFullName(registerType(x.getType.toQuotedString))
        )
      case _: NullConstant =>
        Ast(
          NewLiteral()
            .code("null")
            .typeFullName(registerType("null"))
        )
      case _ =>
        Ast(
          NewLiteral()
            .code(constant.toString)
            .typeFullName(registerType(constant.getType.toQuotedString))
        )
    }
  }

  override def line(node: Host): Option[Int] = {
    if (node == null) None
    else if (node.getJavaSourceStartLineNumber == -1) None
    else Option(node.getJavaSourceStartLineNumber)
  }

  override def column(node: Host): Option[Int] = {
    if (node == null) None
    else if (node.getJavaSourceStartColumnNumber == -1) None
    else Option(node.getJavaSourceStartColumnNumber)
  }

  override def columnEnd(node: Host): Option[Int] = None

  override def lineEnd(node: Host): Option[Int] = None

  override def code(node: Host): String = node.toString

  /** Tracks AST scope.
    */
  protected val stack: mutable.Stack[Ast] = mutable.Stack.empty

}

/** String extensions for strings describing JVM operators.
  */
implicit class JvmStringOpts(s: String) {

  /** Parses the string as a ASM Java type descriptor and returns a fully qualified type. Also converts symbols such as
    * <code>I</code> to <code>int</code>.
    * @return
    */
  def parseAsJavaType: String =
    Try { Type.getType(s).getClassName.replaceAll("/", ".") }.getOrElse(Defines.Any)

}
