package io.shiftleft.fuzzyc2cpg.passes.astcreation

import io.shiftleft.codepropertygraph.generated.nodes.NewNamespaceBlock
import io.shiftleft.fuzzyc2cpg.ast.declarations.ClassDefStatement
import io.shiftleft.fuzzyc2cpg.ast.langc.functiondef.FunctionDef
import io.shiftleft.fuzzyc2cpg.ast.statements.IdentifierDeclStatement
import io.shiftleft.fuzzyc2cpg.ast.walking.ASTNodeVisitor
import io.shiftleft.fuzzyc2cpg.parser.{AntlrParserDriverObserver, TokenSubStream}
import io.shiftleft.fuzzyc2cpg.{Global, ModuleLexer, ModuleParser}
import org.antlr.v4.runtime.tree.ParseTree
import org.antlr.v4.runtime.{CharStream, ParserRuleContext}

class AstVisitor(driver: AntlrCModuleParserDriver, astParentNode: NewNamespaceBlock, global: Global)
    extends ASTNodeVisitor
    with AntlrParserDriverObserver {

  var filenameOption: Option[String] = _
  var childNum: Int = 0

  override def visit(functionDef: FunctionDef): Unit = {
    childNum += 1
    new AstCreator(driver.cpg, astParentNode, global, childNum).convert(functionDef)
  }

  override def visit(classDefStatement: ClassDefStatement): Unit = {
    childNum += 1
    new AstCreator(driver.cpg, astParentNode, global, childNum).convert(classDefStatement)
  }

  override def visit(identifierDeclStmt: IdentifierDeclStatement): Unit = {
    childNum += 1
    new AstCreator(driver.cpg, astParentNode, global, childNum).convert(identifierDeclStmt)
  }

  override def begin(): Unit = {}

  override def end(): Unit = {}

  override def startOfUnit(ctx: ParserRuleContext, filename: String): Unit = {
    filenameOption = Some(filename)
  }

  override def endOfUnit(ctx: ParserRuleContext, filename: String): Unit = {}

  def processItem[T <: io.shiftleft.fuzzyc2cpg.ast.AstNode](
      node: T,
      builderStack: java.util.Stack[
        io.shiftleft.fuzzyc2cpg.ast.AstNodeBuilder[_ <: io.shiftleft.fuzzyc2cpg.ast.AstNode]]): Unit = {
    node.accept(this)
  }

}

class AntlrCModuleParserDriver() extends AntlrParserDriver() {
  setListener(new CModuleParserTreeListener(this))

  override def parseTokenStreamImpl(tokens: TokenSubStream): ParseTree = {
    val parser = new ModuleParser(tokens)
    var tree: ModuleParser.CodeContext = null
    try {
      setSLLMode(parser)
      tree = parser.code
    } catch {
      case ex: RuntimeException =>
        if (isRecognitionException(ex)) {
          tokens.reset()
          setLLStarMode(parser)
          tree = parser.code
        }
    }
    tree
  }

  override def createLexer(input: CharStream) = new ModuleLexer(input)
}
