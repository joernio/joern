package io.shiftleft.fuzzyc2cpg.ast.statements.blockstarters;

import io.shiftleft.fuzzyc2cpg.ast.logical.statements.BlockStarterWithStmtAndCnd;
import io.shiftleft.fuzzyc2cpg.ast.walking.ASTNodeVisitor;

public class WhileStatement extends BlockStarterWithStmtAndCnd {

  @Override
  public void accept(ASTNodeVisitor visitor) {
    visitor.visit(this);
  }
}
