package io.shiftleft.fuzzyc2cpg.ast.expressions;

import io.shiftleft.fuzzyc2cpg.ast.AstNode;
import io.shiftleft.fuzzyc2cpg.ast.walking.ASTNodeVisitor;

public class PostIncDecOperationExpression extends PostfixExpression {

  private Expression variableExpression = null;

  public Expression getVariableExpression() {
    return this.variableExpression;
  }

  public void setVariableExpression(Expression variableExpression) {
    this.variableExpression = variableExpression;
    super.addChild(variableExpression);
  }

  @Override
  public void addChild(AstNode node) {
    if (node instanceof Expression) {
      setVariableExpression((Expression) node);
    } else {
      super.addChild(node);
    }
  }

  @Override
  public void accept(ASTNodeVisitor visitor) {
    visitor.visit(this);
  }
}
