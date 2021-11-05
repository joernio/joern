package io.shiftleft.fuzzyc2cpg.ast.statements.jump;

import io.shiftleft.fuzzyc2cpg.ast.AstNode;
import io.shiftleft.fuzzyc2cpg.ast.expressions.Expression;
import io.shiftleft.fuzzyc2cpg.ast.logical.statements.JumpStatement;
import io.shiftleft.fuzzyc2cpg.ast.walking.ASTNodeVisitor;

public class ThrowStatement extends JumpStatement {

  private Expression throwExpression = null;

  public Expression getThrowExpression() {
    return this.throwExpression;
  }

  public void setThrowExpression(Expression expression) {
    this.throwExpression = expression;
    super.addChild(expression);
  }

  public void accept(ASTNodeVisitor visitor) {
    visitor.visit(this);
  }

  @Override
  public void addChild(AstNode node) {
    if (node instanceof Expression) {
      setThrowExpression((Expression) node);
    } else {
      super.addChild(node);
    }
  }
}
