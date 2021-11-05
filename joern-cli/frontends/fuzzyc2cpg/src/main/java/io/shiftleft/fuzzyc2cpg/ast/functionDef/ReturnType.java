package io.shiftleft.fuzzyc2cpg.ast.functionDef;

import io.shiftleft.fuzzyc2cpg.ast.AstNode;
import io.shiftleft.fuzzyc2cpg.ast.walking.ASTNodeVisitor;

public class ReturnType extends AstNode {

  String completeType;
  String baseType;

  public void setCompleteType(String aCompleteType) {
    completeType = aCompleteType;
  }

  public void setBaseType(String aBaseType) {
    baseType = aBaseType;
  }

  @Override
  public void accept(ASTNodeVisitor visitor) {
    visitor.visit(this);
  }
}
