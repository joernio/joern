package io.joern.rubysrc2cpg.astcreation

import io.joern.rubysrc2cpg.parser.RubyParser.{
  ClassDefinitionPrimaryContext,
  ClassOrModuleReferenceContext,
  ExpressionExpressionOrCommandContext,
  ModuleDefinitionPrimaryContext,
  PrimaryExpressionContext,
  PseudoVariableIdentifierVariableReferenceContext,
  ScopedConstantReferenceContext,
  SelfPseudoVariableIdentifierContext,
  VariableReferencePrimaryContext
}
import io.shiftleft.codepropertygraph.generated.{ModifierTypes, NodeTypes}
import io.joern.rubysrc2cpg.passes.Defines
import io.joern.x2cpg.{Ast, ValidationMode}
import io.shiftleft.codepropertygraph.generated.nodes.*
import org.antlr.v4.runtime.ParserRuleContext
import io.joern.x2cpg.utils.*

import scala.collection.mutable

trait AstForTypesCreator(implicit withSchemaValidation: ValidationMode) { this: AstCreator =>

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

    val className = ctx.className(baseClassName).getOrElse(Defines.Any)
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

      typeDeclNameToTypeDecl.put(className, typeDeclNode)
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
      .code(text(ctx))
    val bodyBlockAst = blockAst(blockNode, astBodyStatement.toList)
    astExprOfCommand ++ Seq(bodyBlockAst)
  }

  def astForModuleDefinitionPrimaryContext(ctx: ModuleDefinitionPrimaryContext): Seq[Ast] = {
    val className = ctx.moduleDefinition().classOrModuleReference().classOrModuleName(None)

    if (className != Defines.Any) {
      classStack.push(className)

      val fullName = classStack.reverse.mkString(pathSep)
      val namespaceBlock = NewNamespaceBlock()
        .name(className)
        .fullName(fullName)
        .filename(filename)

      val moduleBodyAst = astInFakeMethod(className, fullName, filename, ctx)
      classStack.pop()
      Seq(Ast(namespaceBlock).withChildren(moduleBodyAst))
    } else {
      Seq.empty
    }

  }

  private def astInFakeMethod(
    name: String,
    fullName: String,
    path: String,
    ctx: ModuleDefinitionPrimaryContext
  ): Seq[Ast] = {

    val fakeGlobalTypeDecl = NewTypeDecl()
      .name(name)
      .fullName(fullName)

    val bodyAst = astForClassBody(ctx.moduleDefinition().bodyStatement())
    Seq(Ast(fakeGlobalTypeDecl).withChildren(bodyAst))
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
      .collect { case i: NewIdentifier if i.name.startsWith("@") || i.name.isAllUpperCase => i }
      .map { i =>
        val code = ast.root.collect { case c: NewCall => c.code }.getOrElse(i.name)
        val modifierType = i.name match
          case x if x.startsWith("@@") => ModifierTypes.STATIC
          case x if x.isAllUpperCase   => ModifierTypes.FINAL
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

    def className(baseClassName: Option[String] = None): Option[String] =
      Option(ctx.classDefinition().classOrModuleReference()) match {
        case Some(classOrModuleReferenceCtx) =>
          Option(classOrModuleReferenceCtx)
            .map(_.classOrModuleName(baseClassName))
        case None =>
          // TODO the below is just to avoid crashes. This needs to be implemented properly
          None
      }
  }

  implicit class ClassOrModuleReferenceContextExt(val ctx: ClassOrModuleReferenceContext) {

    def hasScopedConstantReference: Boolean = Option(ctx.scopedConstantReference()).isDefined

    def classOrModuleName(baseClassName: Option[String] = None): String =
      Option(ctx) match {
        case Some(ct) =>
          if (ct.hasScopedConstantReference)
            getClassNameScopedConstantReferenceContext(ct.scopedConstantReference())
          else
            Option(ct.CONSTANT_IDENTIFIER()).map(_.getText) match {
              case Some(className) => className
              case None            => Defines.Any
            }
        case None => Defines.Any
      }
  }
}
