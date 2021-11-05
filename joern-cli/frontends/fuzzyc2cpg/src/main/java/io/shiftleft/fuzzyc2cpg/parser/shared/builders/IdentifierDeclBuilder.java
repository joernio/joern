package io.shiftleft.fuzzyc2cpg.parser.shared.builders;

import org.antlr.v4.runtime.ParserRuleContext;
import org.antlr.v4.runtime.tree.ParseTree;

import io.shiftleft.fuzzyc2cpg.ast.declarations.IdentifierDecl;
import io.shiftleft.fuzzyc2cpg.ast.declarations.IdentifierDeclType;
import io.shiftleft.fuzzyc2cpg.ast.expressions.Identifier;
import io.shiftleft.fuzzyc2cpg.parser.AstNodeFactory;
import io.shiftleft.fuzzyc2cpg.parser.ParseTreeUtils;
import io.shiftleft.fuzzyc2cpg.parser.shared.InitDeclContextWrapper;

import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

public class IdentifierDeclBuilder extends TemplateAstBuilder<IdentifierDecl> {

  @Override
  public void createNew(ParserRuleContext ctx) {
    item = new IdentifierDecl();
    AstNodeFactory.initializeFromContext(item, ctx);
  }

  public void setType(InitDeclContextWrapper decl_ctx,
      ParserRuleContext typeName) {
    String baseType = "";
    if (typeName != null) {
      baseType = ParseTreeUtils.childTokenString(typeName);
    }
    String completeType = baseType;
    if (decl_ctx.ptrs() != null) {
      completeType += " "
          + ParseTreeUtils.childTokenString(decl_ctx.ptrs());
    }
    if (decl_ctx.func_ptrs() != null) {
      completeType += " ( "
          + ParseTreeUtils.childTokenString(decl_ctx.func_ptrs()) + " )";
    }
    if (decl_ctx.type_suffix() != null) {
      completeType += " "
          + ParseTreeUtils.childTokenString(decl_ctx.type_suffix());
    }

    // baseType = completeType for function pointers
    if (decl_ctx.func_ptrs() != null) {
      baseType = completeType;
    }

    IdentifierDeclType newType = new IdentifierDeclType();
    AstNodeFactory.initializeFromContext(newType,
        decl_ctx.getWrappedObject());
    newType.baseType = baseType;
    newType.completeType = completeType;
    item.addChild(newType);
  }

  public void setName(InitDeclContextWrapper decl_ctx) {
    ParserRuleContext identifier = decl_ctx.identifier();
    Identifier newName = new Identifier();
    AstNodeFactory.initializeFromContext(newName, identifier);
    item.addChild(newName);
  }

  public List<IdentifierDecl> getDeclarations(ParserRuleContext decl_list,
      ParserRuleContext typeName) {
    List<IdentifierDecl> declarations = new LinkedList<IdentifierDecl>();
    InitDeclContextWrapper decl_ctx;
    for (Iterator<ParseTree> i = decl_list.children.iterator(); i
        .hasNext(); ) {

      decl_ctx = new InitDeclContextWrapper(i.next());
      // for ','s
      if (decl_ctx.getWrappedObject() == null) {
        continue;
      }

      createNew(decl_ctx.getWrappedObject());
      setType(decl_ctx, typeName);
      setName(decl_ctx);

      declarations.add(getItem());
    }
    return declarations;
  }

}
