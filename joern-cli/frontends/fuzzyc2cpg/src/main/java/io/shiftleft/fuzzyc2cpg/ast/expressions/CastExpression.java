package io.shiftleft.fuzzyc2cpg.ast.expressions;

import io.shiftleft.fuzzyc2cpg.ast.AstNode;
import io.shiftleft.fuzzyc2cpg.ast.walking.ASTNodeVisitor;

public class CastExpression extends Expression {

  Expression castTarget = null;
  Expression castExpression = null;

  @Override
  public void addChild(AstNode expression) {
    if (castTarget == null) {
      setCastTarget((Expression) expression);
    } else {
      setCastExpression((Expression) expression);
    }
  }

  public Expression getCastTarget() {
    return this.castTarget;
  }

  public void setCastTarget(Expression castTarget) {
    this.castTarget = castTarget;
    super.addChild(castTarget);
  }

  public Expression getCastExpression() {
    return this.castExpression;
  }

  public void setCastExpression(Expression castExpression) {
    this.castExpression = castExpression;
    super.addChild(castExpression);
  }

  @Override
  public void accept(ASTNodeVisitor visitor) {
    visitor.visit(this);
  }
}
