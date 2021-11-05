package io.shiftleft.fuzzyc2cpg.ast.functionDef;

import io.shiftleft.fuzzyc2cpg.ast.AstNode;
import io.shiftleft.fuzzyc2cpg.ast.logical.statements.CompoundStatement;
import io.shiftleft.fuzzyc2cpg.ast.walking.ASTNodeVisitor;

public abstract class FunctionDefBase extends AstNode {

  protected ParameterList parameterList;
  protected TemplateParameterList templateParameterList;
  protected ReturnType returnType;
  protected CompoundStatement content;

  public abstract String getName();

  public abstract String getFunctionSignature(boolean includeParameterName);

  public ReturnType getReturnType() {
    return returnType;
  }

  public void setReturnType(ReturnType returnType) {
    this.returnType = returnType;
  }

  public ParameterList getParameterList() {
    return this.parameterList;
  }

  public void setParameterList(ParameterList parameterList) {
    this.parameterList = parameterList;
    super.addChild(parameterList);
  }

  public TemplateParameterList getTemplateParameterList() {
    return templateParameterList;
  }

  public void setTemplateParameterList(TemplateParameterList templateParameterList) {
    this.templateParameterList = templateParameterList;
    super.addChild(templateParameterList);
  }

  public CompoundStatement getContent() {
    return this.content;
  }

  public void setContent(CompoundStatement content) {
    this.content = content;
    super.addChild(content);
  }

  @Override
  public String getEscapedCodeStr() {
    setCodeStr(getFunctionSignature(true));
    return getCodeStr();
  }

  @Override
  public void accept(ASTNodeVisitor visitor) {
    visitor.visit(this);
  }
}
