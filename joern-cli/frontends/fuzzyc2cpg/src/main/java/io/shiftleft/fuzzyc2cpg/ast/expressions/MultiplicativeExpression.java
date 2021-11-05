package io.shiftleft.fuzzyc2cpg.ast.expressions;

import io.shiftleft.fuzzyc2cpg.ast.walking.ASTNodeVisitor;

public class MultiplicativeExpression extends BinaryOperationExpression {
  public void accept(ASTNodeVisitor visitor) {
    visitor.visit(this);
  }
}
