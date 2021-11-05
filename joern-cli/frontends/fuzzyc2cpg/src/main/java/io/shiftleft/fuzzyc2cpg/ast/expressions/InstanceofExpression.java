package io.shiftleft.fuzzyc2cpg.ast.expressions;

import io.shiftleft.fuzzyc2cpg.ast.walking.ASTNodeVisitor;

public class InstanceofExpression extends Expression {

  Expression instanceExpression = null;
  Expression classExpression = null;

  public Expression getInstanceExpression() {
    return this.instanceExpression;
  }

  public void setInstanceExpression(Expression instanceExpression) {
    this.instanceExpression = instanceExpression;
    super.addChild(instanceExpression);
  }

  public Expression getClassExpression() {
    return this.classExpression;
  }

  public void setClassExpression(Expression classExpression) {
    this.classExpression = classExpression;
    super.addChild(classExpression);
  }

  @Override
  public void accept(ASTNodeVisitor visitor) {
    visitor.visit(this);
  }
}
