package io.joern.rust2cpg.astcreation

import flatgraph.DiffGraphBuilder
import io.joern.rust2cpg.Config
import io.joern.rust2cpg.parser.RustJsonParser.{ParseResult, isMacroExpanded}
import io.joern.rust2cpg.parser.RustNodeSyntax
import io.joern.rust2cpg.parser.RustNodeSyntax.RustNode
import io.joern.x2cpg.datastructures.Stack.*
import io.joern.x2cpg.{Ast, AstCreatorBase, ValidationMode}
import io.shiftleft.codepropertygraph.generated.nodes.{
  NewCall,
  NewControlStructure,
  NewMethod,
  NewNamespaceBlock,
  NewNode,
  NewTypeDecl
}
import io.shiftleft.codepropertygraph.generated.{NodeTypes, Operators, PropertyDefaults, PropertyNames}
import io.shiftleft.semanticcpg.language.types.structure.NamespaceTraversal
import org.slf4j.LoggerFactory

import java.nio.charset.StandardCharsets

class AstCreator(val config: Config, val parseResult: ParseResult)(implicit withSchemaValidation: ValidationMode)
    extends AstCreatorBase[RustNode, AstCreator](parseResult.filename)
    with RustVisitor
    with RustFullNames {

  private val logger = LoggerFactory.getLogger(getClass)

  protected val methodAstParentStack = new Stack[NewNode]

  override def createAst(): DiffGraphBuilder = {
    val sourceFile = parseResult.ast.asInstanceOf[RustNodeSyntax.SourceFile]
    val ast        = visitSourceFile(sourceFile)
    Ast.storeInDiffGraph(ast, diffGraph)
    diffGraph
  }

  // NB: rust_ast_gen uses 0-based line/column
  override protected def line(node: RustNode): Option[Int] =
    if (node.isMacroExpanded) None else node.startLine.map(_ + 1)
  override protected def column(node: RustNode): Option[Int] =
    if (node.isMacroExpanded) None else node.startColumn.map(_ + 1)

  override protected def lineEnd(node: RustNode): Option[Int]   = None
  override protected def columnEnd(node: RustNode): Option[Int] = None
  override protected def code(node: RustNode): String = text(node).map(shortenCode(_)).getOrElse(PropertyDefaults.Code)

  protected def text(node: RustNode): Option[String] = if (node.isMacroExpanded) {
    node.text
  } else {
    (node.startOffset, node.endOffset) match {
      case (Some(start), Some(end)) => Some(String(parseResult.contentBytes.slice(start, end), StandardCharsets.UTF_8))
      case _                        => None
    }
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

  protected def macroNotExpanded(macroCall: RustNodeSyntax.MacroCall): Ast = {
    val name = code(macroCall.path)
    val text =
      s"""Macro '$name' not expanded!
         |  Code: '${code(macroCall)}'
         |  File: '${parseResult.fullPath}'
         |  Line: ${line(macroCall).getOrElse(-1)}
         |  Column: ${column(macroCall).getOrElse(-1)}
         |  """.stripMargin
    logger.warn(text)
    Ast(unknownNode(macroCall, code(macroCall)))
  }

  protected def assignmentNode(node: RustNodeSyntax.RustNode, code: String): NewCall = {
    operatorCallNode(node = node, name = Operators.assignment, code = code, typeFullName = None)
  }

  protected def methodNode(node: RustNode, name: String): NewMethod = {
    methodNode(
      node = node,
      name = name,
      code = code(node),
      fullName = composeRustFullName(name),
      signature = Some(""),
      fileName = parseResult.filename,
      astParentType = Some(methodAstParentStack.head.label),
      astParentFullName = Some(methodAstParentStack.head.properties(PropertyNames.FullName).toString)
    )
  }

  protected def globalNamespaceBlockNode(): NewNamespaceBlock = {
    NewNamespaceBlock()
      .name(rustNamespaceFullName)
      .fullName(globalNamespaceFullName)
      .filename(parseResult.filename)
      .order(1)
  }

  private def globalNamespaceFullName: String = {
    s"${parseResult.filename}:$rustNamespaceFullName"
  }

  protected def moduleNamespaceBlockNode(module: RustNodeSyntax.Module): NewNamespaceBlock = {
    val name = composeRustFullName(code(module.name))
    namespaceBlockNode(module, name, s"${parseResult.filename}:$name", parseResult.filename)
  }

  protected def globalFakeMethodNode(
    sourceFile: RustNodeSyntax.SourceFile,
    namespaceBlock: NewNamespaceBlock
  ): NewMethod = {
    val name = NamespaceTraversal.globalNamespaceName
    methodNode(
      node = sourceFile,
      name = name,
      code = name,
      fullName = combineRustFullName(namespaceBlock.fullName, name),
      signature = Some(""),
      fileName = parseResult.filename,
      astParentType = Some(NodeTypes.NAMESPACE_BLOCK),
      astParentFullName = Some(namespaceBlock.fullName)
    )
  }

  protected def typeDeclForStruct(struct: RustNodeSyntax.Struct): NewTypeDecl = {
    val name   = code(struct.name)
    val parent = methodAstParentStack.head
    typeDeclNode(
      node = struct,
      name = name,
      fullName = composeRustFullName(name),
      filename = parseResult.filename,
      code = code(struct),
      astParentType = parent.label,
      astParentFullName = parent.properties(PropertyNames.FullName).toString
    )
  }

  protected def typeDeclForImpl(impl: RustNodeSyntax.Impl): NewTypeDecl = {
    val implType = typeFullNameForType(impl.typ.last)
    val name     = implType.split(RustFullNames.PathSep).lastOption.getOrElse(implType)
    val parent   = methodAstParentStack.head
    typeDeclNode(
      node = impl,
      name = name,
      fullName = implType,
      filename = parseResult.filename,
      code = code(impl),
      astParentType = parent.label,
      astParentFullName = parent.properties(PropertyNames.FullName).toString
    )
  }

  protected def enclosingTypeDeclFullName: Option[String] = {
    methodAstParentStack.collectFirst { case typeDecl: NewTypeDecl =>
      typeDecl.properties(PropertyNames.FullName).toString
    }
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

  protected def whileBodyAst(whileNode: NewControlStructure, conditionAst: Ast, bodyAst: Ast): Ast = {
    val ast = controlStructureAst(whileNode, Some(conditionAst), Seq(bodyAst))
    bodyAst.root match {
      case Some(bodyRoot) => ast.withTrueBodyEdge(whileNode, bodyRoot)
      case None           => ast
    }
  }

}
