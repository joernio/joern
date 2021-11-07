package io.joern.fuzzyc2cpg.ast.logical.statements;

import io.joern.fuzzyc2cpg.ast.expressions.IntegerExpression;
import io.joern.fuzzyc2cpg.ast.walking.ASTNodeVisitor;

public class BreakOrContinueStatement extends JumpStatement {
  @Override
  public void accept(ASTNodeVisitor visitor) {
    visitor.visit(this);
  }
}
