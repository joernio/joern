package io.shiftleft.fuzzyc2cpg.ast.statements.blockstarters;

import io.shiftleft.fuzzyc2cpg.ast.AstNode;
import io.shiftleft.fuzzyc2cpg.ast.logical.statements.BlockStarter;
import io.shiftleft.fuzzyc2cpg.ast.logical.statements.CompoundStatement;
import io.shiftleft.fuzzyc2cpg.ast.walking.ASTNodeVisitor;

public class TryStatement extends BlockStarter {

  private CompoundStatement content = null;
  private CatchList catchList;
  private CompoundStatement finallyContent = null;

  public CompoundStatement getContent() {
    return this.content;
  }

  public void setContent(CompoundStatement content) {
    this.content = content;
    super.addChild(content);
  }

  public CatchList getCatchList() {
    return this.catchList;
  }

  public void setCatchList(CatchList catchList) {
    this.catchList = catchList;
    super.addChild(catchList);
  }

  public CompoundStatement getFinallyContent() {
    return this.finallyContent;
  }

  public void setFinallyContent(CompoundStatement finallyContent) {
    this.finallyContent = finallyContent;
    super.addChild(finallyContent);
  }

  @Override
  public void accept(ASTNodeVisitor visitor) {
    visitor.visit(this);
  }

  @Override
  public void addChild(AstNode node) {
    if (node instanceof CompoundStatement && getChildCount() == 0) {
      setContent((CompoundStatement) node);
    } else if (node instanceof CatchList) {
      setCatchList((CatchList) node);
    } else if (node instanceof CompoundStatement && getChildCount() == 2) {
      setFinallyContent((CompoundStatement) node);
    } else {
      super.addChild(node);
    }
  }
}
