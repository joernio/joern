package io.shiftleft.fuzzyc2cpg.ast.langc.functiondef;

import io.shiftleft.fuzzyc2cpg.ast.AstNode;
import io.shiftleft.fuzzyc2cpg.ast.expressions.Identifier;
import io.shiftleft.fuzzyc2cpg.ast.functionDef.FunctionDefBase;
import io.shiftleft.fuzzyc2cpg.ast.functionDef.ParameterList;
import io.shiftleft.fuzzyc2cpg.ast.functionDef.Template;
import io.shiftleft.fuzzyc2cpg.ast.functionDef.TemplateParameterList;
import io.shiftleft.fuzzyc2cpg.ast.logical.statements.CompoundStatement;
import io.shiftleft.fuzzyc2cpg.ast.walking.ASTNodeVisitor;

public class FunctionDef extends FunctionDefBase {

  private Identifier identifier = null;
  private boolean isOnlyDeclaration = false;

  public Identifier getIdentifier() {
    return this.identifier;
  }

  private void setIdentifier(Identifier identifier) {
    this.identifier = identifier;
    super.addChild(identifier);
  }

  @Override
  public String getName() {
    return this.getIdentifier().getEscapedCodeStr();
  }

  @Override
  public String getFunctionSignature(boolean includeParameterName) {
    StringBuilder sb = new StringBuilder();
    sb.append(getIdentifier().getEscapedCodeStr());
    if (getParameterList() != null) {
      sb.append(" (").append(getParameterList().getEscapedCodeStr(includeParameterName)).append(")");
    } else {
      sb.append(" ()");
    }
    return sb.toString();
  }

  @Override
  public void addChild(AstNode node) {
    if (node instanceof CompoundStatement) {
      setContent((CompoundStatement) node);
    } else if (node instanceof ParameterList) {
      setParameterList((ParameterList) node);
    } else if (node instanceof TemplateParameterList) {
      setTemplateParameterList((TemplateParameterList) node);
    } else if (node instanceof Identifier) {
      setIdentifier((Identifier) node);
    } else {
      super.addChild(node);
    }
  }

  @Override
  public void accept(ASTNodeVisitor visitor) {
    visitor.visit(this);
  }

  public void setIsOnlyDeclaration(boolean isOnlyDeclaration) {
    this.isOnlyDeclaration = isOnlyDeclaration;
  }

  public boolean isOnlyDeclaration() {
    return this.isOnlyDeclaration;
  }

}
