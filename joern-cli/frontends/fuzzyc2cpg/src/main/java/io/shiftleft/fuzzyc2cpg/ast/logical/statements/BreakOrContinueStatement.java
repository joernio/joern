package io.shiftleft.fuzzyc2cpg.ast.logical.statements;

import io.shiftleft.fuzzyc2cpg.ast.expressions.IntegerExpression;
import io.shiftleft.fuzzyc2cpg.ast.walking.ASTNodeVisitor;

public class BreakOrContinueStatement extends JumpStatement {
  @Override
  public void accept(ASTNodeVisitor visitor) {
    visitor.visit(this);
  }
}
