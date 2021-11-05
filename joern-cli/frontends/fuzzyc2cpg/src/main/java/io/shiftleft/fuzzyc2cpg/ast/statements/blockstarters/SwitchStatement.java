package io.shiftleft.fuzzyc2cpg.ast.statements.blockstarters;

import io.shiftleft.fuzzyc2cpg.ast.logical.statements.BlockStarterWithStmtAndCnd;
import io.shiftleft.fuzzyc2cpg.ast.walking.ASTNodeVisitor;

public class SwitchStatement extends BlockStarterWithStmtAndCnd {

  public void accept(ASTNodeVisitor visitor) {
    visitor.visit(this);
  }
}
