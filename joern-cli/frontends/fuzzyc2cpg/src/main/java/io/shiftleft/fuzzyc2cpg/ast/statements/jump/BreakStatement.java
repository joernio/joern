package io.shiftleft.fuzzyc2cpg.ast.statements.jump;

import io.shiftleft.fuzzyc2cpg.ast.logical.statements.BreakOrContinueStatement;
import io.shiftleft.fuzzyc2cpg.ast.walking.ASTNodeVisitor;

public class BreakStatement extends BreakOrContinueStatement {

  @Override
  public void accept(ASTNodeVisitor visitor) {
    visitor.visit(this);
  }
}
