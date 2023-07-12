package io.joern.rubysrc2cpg.astcreation

import io.joern.rubysrc2cpg.parser.RubyParser.{
  ClassDefinitionPrimaryContext,
  ClassOrModuleReferenceContext,
  ScopedConstantReferenceContext
}
import io.shiftleft.codepropertygraph.generated.ModifierTypes
import io.joern.rubysrc2cpg.passes.Defines
import io.joern.x2cpg.Ast
import io.shiftleft.codepropertygraph.generated.nodes.*
import org.antlr.v4.runtime.ParserRuleContext

import scala.collection.mutable

trait AstForTypesCreator { this: AstCreator =>

  // Maps field references of known types
  protected val fieldReferences = mutable.HashMap.empty[String, Set[ParserRuleContext]]

  def astForClassDeclaration(ctx: ClassDefinitionPrimaryContext): Seq[Ast] = {
    val baseClassName = if (ctx.classDefinition().expressionOrCommand() != null) {
      val parentClassNameAst = astForExpressionOrCommand(ctx.classDefinition().expressionOrCommand())
      val nameNode = parentClassNameAst.head.nodes
        .filter(node => node.isInstanceOf[NewIdentifier])
        .head
        .asInstanceOf[NewIdentifier]
      Some(nameNode.name)
    } else {
      None
    }

    val className = ctx.className(baseClassName)
    if (className != Defines.Any) {
      classStack.push(className)
      val fullName = classStack.reverse.mkString(pathSep)

      val bodyAst = astForClassBody(ctx.classDefinition().bodyStatement())

      if (classStack.nonEmpty) {
        classStack.pop()
      }

      val typeDeclNode = NewTypeDecl()
        .name(className)
        .fullName(fullName)
      Seq(Ast(typeDeclNode).withChildren(bodyAst))
    } else {
      Seq.empty
    }
  }

  def astForClassExpression(ctx: ClassDefinitionPrimaryContext): Seq[Ast] = {
    // TODO test for this is pending due to lack of understanding to generate an example
    val astExprOfCommand = astForExpressionOrCommand(ctx.classDefinition().expressionOrCommand())
    val astBodyStatement = astForBodyStatementContext(ctx.classDefinition().bodyStatement())
    val blockNode = NewBlock()
      .code(ctx.getText)
    val bodyBlockAst = blockAst(blockNode, astBodyStatement.toList)
    astExprOfCommand ++ Seq(bodyBlockAst)
  }

  def astForClassOrModuleReferenceContext(
    ctx: ClassOrModuleReferenceContext,
    baseClassName: Option[String] = None
  ): Seq[Ast] = {
    val className = ctx.className(baseClassName)

    if (className != Defines.Any) {
      classStack.push(className)
    }
    Seq(Ast())
  }

  private def getClassNameScopedConstantReferenceContext(ctx: ScopedConstantReferenceContext): String = {
    val classTerminalNode = ctx.CONSTANT_IDENTIFIER()

    if (ctx.primary() != null) {
      val primaryAst = astForPrimaryContext(ctx.primary())
      val moduleNameNode = primaryAst.head.nodes
        .filter(node => node.isInstanceOf[NewIdentifier])
        .head
        .asInstanceOf[NewIdentifier]
      val moduleName = moduleNameNode.name
      moduleName + "." + classTerminalNode.getText
    } else {
      classTerminalNode.getText
    }
  }

  def membersFromStatementAsts(ast: Ast): Seq[Ast] =
    ast.nodes
      .collect { case i: NewIdentifier if i.name.startsWith("@") => i }
      .map { i =>
        val code = ast.root.collect { case c: NewCall => c.code }.getOrElse(i.name)
        val modifierType = i.name match
          case x if x.startsWith("@@") => ModifierTypes.STATIC
          case _                       => ModifierTypes.VIRTUAL
        val modifierAst = Ast(NewModifier().modifierType(modifierType))
        Ast(
          NewMember()
            .code(code)
            .name(i.name.replaceAll("@", ""))
            .typeFullName(i.typeFullName)
            .lineNumber(i.lineNumber)
            .columnNumber(i.columnNumber)
        ).withChild(modifierAst)
      }
      .toSeq

  implicit class ClassDefinitionPrimaryContextExt(val ctx: ClassDefinitionPrimaryContext) {

    def hasClassDefinition: Boolean = Option(ctx.classDefinition()).isDefined

    def className(baseClassName: Option[String] = None): String =
      Option(ctx.classDefinition())
        .map(_.classOrModuleReference())
        .map(_.className(baseClassName))
        .getOrElse(Defines.Any)
  }

  implicit class ClassOrModuleReferenceContextExt(val ctx: ClassOrModuleReferenceContext) {

    def hasScopedConstantReference: Boolean = Option(ctx.scopedConstantReference()).isDefined

    def className(baseClassName: Option[String] = None): String =
      if (ctx.hasScopedConstantReference)
        getClassNameScopedConstantReferenceContext(ctx.scopedConstantReference())
      else
        Option(ctx.CONSTANT_IDENTIFIER()).map(_.getText) match {
          case Some(className) if baseClassName.isDefined => s"${baseClassName.get}.$className"
          case Some(className)                            => className
          case None                                       => Defines.Any
        }

  }

}
