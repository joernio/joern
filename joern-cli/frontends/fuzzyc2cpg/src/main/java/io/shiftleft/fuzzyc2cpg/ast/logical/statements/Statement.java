package io.shiftleft.fuzzyc2cpg.ast.logical.statements;

import io.shiftleft.fuzzyc2cpg.ast.AstNode;
import io.shiftleft.fuzzyc2cpg.ast.walking.ASTNodeVisitor;

public class Statement extends AstNode {
  @Override
  public void accept(ASTNodeVisitor visitor) {
    visitor.visit(this);
  }
}
