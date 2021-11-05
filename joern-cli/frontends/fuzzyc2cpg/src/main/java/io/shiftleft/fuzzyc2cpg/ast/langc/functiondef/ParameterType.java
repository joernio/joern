package io.shiftleft.fuzzyc2cpg.ast.langc.functiondef;

import io.shiftleft.fuzzyc2cpg.ast.AstNode;
import io.shiftleft.fuzzyc2cpg.ast.walking.ASTNodeVisitor;

public class ParameterType extends AstNode {

  String completeType = "";
  String baseType = "";

  @Override
  public String getEscapedCodeStr() {
    setCodeStr(completeType);
    return getCodeStr();
  }

  public void setCompleteType(String completeType) {
    this.completeType = completeType;
  }

  public void setBaseType(String baseType) {
    this.baseType = baseType;
  }

  @Override
  public void accept(ASTNodeVisitor visitor) {
    visitor.visit(this);
  }
}
