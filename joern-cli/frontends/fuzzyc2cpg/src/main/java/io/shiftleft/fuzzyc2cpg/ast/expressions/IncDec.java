package io.shiftleft.fuzzyc2cpg.ast.expressions;

// This isn't nice because an operator is
// not a standalone expression and can't
// be evaluated.

import io.shiftleft.fuzzyc2cpg.ast.walking.ASTNodeVisitor;

public class IncDec extends Expression {
  @Override
  public void accept(ASTNodeVisitor visitor) {
    visitor.visit(this);
  }

}
