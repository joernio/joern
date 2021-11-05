package io.shiftleft.fuzzyc2cpg.ast.expressions;

import io.shiftleft.fuzzyc2cpg.ast.walking.ASTNodeVisitor;

public class ConditionalExpression extends Expression {

  protected Expression condition = null;
  protected Expression trueExpression = null;
  protected Expression falseExpression = null;

  public Expression getCondition() {
    return this.condition;
  }

  public void setCondition(Expression expression) {
    this.condition = expression;
    super.addChild(expression);
  }

  public Expression getTrueExpression() {
    return this.trueExpression;
  }

  public void setTrueExpression(Expression trueExpression) {
    this.trueExpression = trueExpression;
    super.addChild(trueExpression);
  }

  public Expression getFalseExpression() {
    return this.falseExpression;
  }

  public void setFalseExpression(Expression falseExpression) {
    this.falseExpression = falseExpression;
    super.addChild(falseExpression);
  }

  @Override
  public void accept(ASTNodeVisitor visitor) {
    visitor.visit(this);
  }
}
