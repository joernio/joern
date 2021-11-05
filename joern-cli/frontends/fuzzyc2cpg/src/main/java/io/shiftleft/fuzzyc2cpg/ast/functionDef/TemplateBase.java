package io.shiftleft.fuzzyc2cpg.ast.functionDef;

import io.shiftleft.fuzzyc2cpg.ast.AstNode;
import io.shiftleft.fuzzyc2cpg.ast.walking.ASTNodeVisitor;

public abstract class TemplateBase extends AstNode {
  public abstract String getName();

  @Override
  public void accept(ASTNodeVisitor visitor) {
    visitor.visit(this);
  }
}
