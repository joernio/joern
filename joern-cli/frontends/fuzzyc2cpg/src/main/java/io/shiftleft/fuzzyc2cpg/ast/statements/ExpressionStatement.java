package io.shiftleft.fuzzyc2cpg.ast.statements;

import io.shiftleft.fuzzyc2cpg.ast.walking.ASTNodeVisitor;

public class ExpressionStatement extends ExpressionHolderStatement {
  @Override
  public void accept(ASTNodeVisitor visitor) {
    visitor.visit(this);
  }

}
