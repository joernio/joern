package io.shiftleft.fuzzyc2cpg.ast.statements.jump;

import io.shiftleft.fuzzyc2cpg.ast.AstNode;
import io.shiftleft.fuzzyc2cpg.ast.expressions.Expression;
import io.shiftleft.fuzzyc2cpg.ast.logical.statements.JumpStatement;
import io.shiftleft.fuzzyc2cpg.ast.walking.ASTNodeVisitor;

public class ReturnStatement extends JumpStatement {

  private Expression returnExpression = null;

  public Expression getReturnExpression() {
    return this.returnExpression;
  }

  public void setReturnExpression(Expression expression) {
    this.returnExpression = expression;
    super.addChild(expression);
  }

  public void accept(ASTNodeVisitor visitor) {
    visitor.visit(this);
  }

  @Override
  public void addChild(AstNode node) {
    if (node instanceof Expression) {
      setReturnExpression((Expression) node);
    } else {
      super.addChild(node);
    }
  }
}
