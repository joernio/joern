package io.joern.rust2cpg.astcreation

import flatgraph.DiffGraphBuilder
import io.joern.rust2cpg.Config
import io.joern.rust2cpg.parser.RustJsonParser.ParseResult
import io.joern.rust2cpg.parser.RustNodeSyntax
import io.joern.rust2cpg.parser.RustNodeSyntax.RustNode
import io.joern.x2cpg.datastructures.Stack.*
import io.joern.x2cpg.passes.frontend.MetaDataPass
import io.joern.x2cpg.{Ast, AstCreatorBase, Defines, ValidationMode}
import io.shiftleft.codepropertygraph.generated.nodes.{NewCall, NewMethod, NewNode}
import io.shiftleft.codepropertygraph.generated.{NodeTypes, Operators, PropertyDefaults, PropertyNames}
import io.shiftleft.semanticcpg.language.types.structure.NamespaceTraversal
import org.slf4j.LoggerFactory

import java.nio.charset.StandardCharsets

class AstCreator(val config: Config, val parseResult: ParseResult)(implicit withSchemaValidation: ValidationMode)
    extends AstCreatorBase[RustNode, AstCreator](parseResult.filename)
    with RustVisitor {

  private val logger = LoggerFactory.getLogger(getClass)

  protected val methodAstParentStack = new Stack[NewNode]

  override def createAst(): DiffGraphBuilder = {
    val sourceFile = parseResult.ast.asInstanceOf[RustNodeSyntax.SourceFile]
    val ast        = visitSourceFile(sourceFile)
    Ast.storeInDiffGraph(ast, diffGraph)
    diffGraph
  }

  // NB: rust_ast_gen uses 0-based line/column
  override protected def line(node: RustNode): Option[Int]      = node.startLine.map(_ + 1)
  override protected def column(node: RustNode): Option[Int]    = node.startColumn.map(_ + 1)
  override protected def lineEnd(node: RustNode): Option[Int]   = None
  override protected def columnEnd(node: RustNode): Option[Int] = None
  override protected def code(node: RustNode): String = text(node).map(shortenCode(_)).getOrElse(PropertyDefaults.Code)

  protected def text(node: RustNode): Option[String] = (node.startOffset, node.endOffset) match {
    case (Some(start), Some(end)) => Some(String(parseResult.contentBytes.slice(start, end), StandardCharsets.UTF_8))
    case _                        => None
  }

  protected def globalMethodNode(): NewMethod = {
    val name     = NamespaceTraversal.globalNamespaceName
    val fullName = MetaDataPass.getGlobalNamespaceBlockFullName(Some(parseResult.filename))
    NewMethod()
      .name(name)
      .code(name)
      .fullName(fullName)
      .filename(parseResult.filename)
      .astParentType(NodeTypes.NAMESPACE_BLOCK)
      .astParentFullName(fullName)
  }

  protected def notHandledYet(node: RustNode): Ast = {
    val text =
      s"""Node type '${node.getClass.getSimpleName}' not handled yet!
         |  Code: '${code(node)}'
         |  File: '${parseResult.fullPath}'
         |  Line: ${line(node).getOrElse(-1)}
         |  Column: ${column(node).getOrElse(-1)}
         |  """.stripMargin
    logger.warn(text)
    Ast(unknownNode(node, code(node)))
  }

  protected def assignmentNode(node: RustNodeSyntax.RustNode, code: String): NewCall = {
    operatorCallNode(node = node, name = Operators.assignment, code = code, typeFullName = None)
  }

  private def composeMethodFullName(name: String): String = {
    // TODO
    val astParentFullName = methodAstParentStack.head.properties(PropertyNames.FullName).toString
    formatMethodFullName(Seq(astParentFullName, name))
  }

  private def formatMethodFullName(names: Seq[String]): String = {
    // TODO
    names.mkString(".")
  }

  protected def methodNode(node: RustNodeSyntax.RustNode, name: String): NewMethod = {
    // TODO
    val methodFullName  = composeMethodFullName(name)
    val methodSignature = ""
    methodNode(
      node = node,
      name = name,
      fullName = methodFullName,
      signature = methodSignature,
      fileName = parseResult.filename
    )
  }

  // TODO
  protected def typeFullNameForType(typ: RustNodeSyntax.Type): String = {
    text(typ).getOrElse(Defines.Any)
  }

  // TODO
  protected def typeFullNameForNameRef(nameRef: RustNodeSyntax.NameRef): String = {
    Defines.Any
  }

  // TODO
  protected def typeFullNameForPath(path: RustNodeSyntax.Path): String = {
    Defines.Any
  }

  protected def typeFullNameForLiteral(lit: RustNodeSyntax.Literal): String = {
    lit.value.map(typeFullNameForLiteralToken).getOrElse(Defines.Any)
  }

  protected def typeFullNameForLiteralToken(tok: RustNodeSyntax.RustToken): String = tok match {
    case _: RustNodeSyntax.IntNumberToken   => "i32"
    case _: RustNodeSyntax.FloatNumberToken => "f64"
    case _: RustNodeSyntax.StringToken      => "&str"
    case _: RustNodeSyntax.ByteStringToken  => "&[u8]"
    case _: RustNodeSyntax.CStringToken     => "&CStr"
    case _: RustNodeSyntax.CharToken        => "char"
    case _: RustNodeSyntax.ByteToken        => "u8"
    case _: RustNodeSyntax.TrueKwToken      => "bool"
    case _: RustNodeSyntax.FalseKwToken     => "bool"
    case _                                  => Defines.Any
  }

  protected def operatorNameFor(binExpr: RustNodeSyntax.BinExpr): Option[String] = binExpr.op match {
    case Some(_: RustNodeSyntax.Pipe2Token)     => Some(Operators.logicalOr)
    case Some(_: RustNodeSyntax.Amp2Token)      => Some(Operators.logicalAnd)
    case Some(_: RustNodeSyntax.Eq2Token)       => Some(Operators.equals)
    case Some(_: RustNodeSyntax.NeqToken)       => Some(Operators.notEquals)
    case Some(_: RustNodeSyntax.LteqToken)      => Some(Operators.lessEqualsThan)
    case Some(_: RustNodeSyntax.GteqToken)      => Some(Operators.greaterEqualsThan)
    case Some(_: RustNodeSyntax.LAngleToken)    => Some(Operators.lessThan)
    case Some(_: RustNodeSyntax.RAngleToken)    => Some(Operators.greaterThan)
    case Some(_: RustNodeSyntax.PlusToken)      => Some(Operators.addition)
    case Some(_: RustNodeSyntax.StarToken)      => Some(Operators.multiplication)
    case Some(_: RustNodeSyntax.MinusToken)     => Some(Operators.subtraction)
    case Some(_: RustNodeSyntax.SlashToken)     => Some(Operators.division)
    case Some(_: RustNodeSyntax.PercentToken)   => Some(Operators.modulo)
    case Some(_: RustNodeSyntax.ShlToken)       => Some(Operators.shiftLeft)
    case Some(_: RustNodeSyntax.ShrToken)       => Some(Operators.arithmeticShiftRight)
    case Some(_: RustNodeSyntax.CaretToken)     => Some(Operators.xor)
    case Some(_: RustNodeSyntax.PipeToken)      => Some(Operators.or)
    case Some(_: RustNodeSyntax.AmpToken)       => Some(Operators.and)
    case Some(_: RustNodeSyntax.EqToken)        => Some(Operators.assignment)
    case Some(_: RustNodeSyntax.PluseqToken)    => Some(Operators.assignmentPlus)
    case Some(_: RustNodeSyntax.SlasheqToken)   => Some(Operators.assignmentDivision)
    case Some(_: RustNodeSyntax.StareqToken)    => Some(Operators.assignmentMultiplication)
    case Some(_: RustNodeSyntax.PercenteqToken) => Some(Operators.assignmentModulo)
    case Some(_: RustNodeSyntax.ShreqToken)     => Some(Operators.assignmentArithmeticShiftRight)
    case Some(_: RustNodeSyntax.ShleqToken)     => Some(Operators.assignmentShiftLeft)
    case Some(_: RustNodeSyntax.MinuseqToken)   => Some(Operators.assignmentMinus)
    case Some(_: RustNodeSyntax.PipeeqToken)    => Some(Operators.assignmentOr)
    case Some(_: RustNodeSyntax.AmpeqToken)     => Some(Operators.assignmentAnd)
    case Some(_: RustNodeSyntax.CareteqToken)   => Some(Operators.assignmentXor)
    case _                                      => None
  }

  protected def operatorNameFor(prefixExpr: RustNodeSyntax.PrefixExpr): Option[String] = prefixExpr.op match {
    case Some(_: RustNodeSyntax.MinusToken) => Some(Operators.minus)
    case Some(_: RustNodeSyntax.BangToken)  => Some(Operators.logicalNot)
    case Some(_: RustNodeSyntax.StarToken)  => Some(Operators.indirection)
    case _                                  => None
  }

  protected def methodFullNameForCallExpr(nameRefs: Seq[RustNodeSyntax.NameRef]): String = {
    // TODO
    nameRefs.map(code) match {
      case Nil   => Defines.Unknown
      case names => formatMethodFullName(names)
    }
  }

  protected def methodFullNameForMethodCallExpr(methodCallExpr: RustNodeSyntax.MethodCallExpr): String = {
    // TODO
    Defines.DynamicCallUnknownFullName
  }

  protected def typeFullNameForMethodCallExpr(methodCallExpr: RustNodeSyntax.MethodCallExpr): String = {
    // TODO
    Defines.Any
  }

}
