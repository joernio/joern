package io.shiftleft.fuzzyc2cpg.ast.statements.jump;

import io.shiftleft.fuzzyc2cpg.ast.logical.statements.JumpStatement;
import io.shiftleft.fuzzyc2cpg.ast.walking.ASTNodeVisitor;

public class GotoStatement extends JumpStatement {

  public String getTargetName() {
    // TODO since C world does not use the setTargetLabel() method but
    // instead uses addChild(), we have to use getChild(0) here
    // instead of getTargetLabel()
    return getChild(0).getEscapedCodeStr();
  }

  public String getEscapedCodeStr() {
    return "goto " + getTargetName() + ";";
  }

  public void accept(ASTNodeVisitor visitor) {
    visitor.visit(this);
  }
}
