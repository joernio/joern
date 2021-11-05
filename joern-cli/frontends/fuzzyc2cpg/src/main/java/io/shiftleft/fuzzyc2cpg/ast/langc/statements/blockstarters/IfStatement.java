package io.shiftleft.fuzzyc2cpg.ast.langc.statements.blockstarters;

import io.shiftleft.fuzzyc2cpg.ast.AstNode;
import io.shiftleft.fuzzyc2cpg.ast.statements.blockstarters.IfStatementBase;
import io.shiftleft.fuzzyc2cpg.ast.walking.ASTNodeVisitor;

public class IfStatement extends IfStatementBase {

  private ElseStatement elseNode = null;

  public int getChildCount() {
    int childCount = super.getChildCount();

    if (getElseNode() != null) {
      childCount++;
    }
    return childCount;
  }

  public AstNode getChild(int index) {
    if (index == 0) {
      return condition;
    } else if (index == 1) {
      return statement;
    } else if (index == 2) {
      return getElseNode();
    }
    throw new RuntimeException("Invalid IfItem");
  }

  public ElseStatement getElseNode() {
    return elseNode;
  }

  public void setElseNode(ElseStatement elseNode) {
    this.elseNode = elseNode;
  }

  public void accept(ASTNodeVisitor visitor) {
    visitor.visit(this);
  }
}
