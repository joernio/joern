package io.shiftleft.fuzzyc2cpg.ast.expressions;

import io.shiftleft.fuzzyc2cpg.ast.statements.ExpressionHolder;
import io.shiftleft.fuzzyc2cpg.ast.walking.ASTNodeVisitor;

// Pseudo node
public class Callee extends ExpressionHolder {
  @Override
  public void accept(ASTNodeVisitor visitor) {
    visitor.visit(this);
  }

}
