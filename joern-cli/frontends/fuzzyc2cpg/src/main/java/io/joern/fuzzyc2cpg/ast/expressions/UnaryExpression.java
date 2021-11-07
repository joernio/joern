package io.joern.fuzzyc2cpg.ast.expressions;

import io.joern.fuzzyc2cpg.ast.AstNode;
import io.joern.fuzzyc2cpg.ast.walking.ASTNodeVisitor;

public class UnaryExpression extends Expression {

  Expression expression = null;

  public Expression getExpression() {
    return this.expression;
  }

  public void setExpression(Expression expression) {
    this.expression = expression;
    super.addChild(expression);
  }

  public void accept(ASTNodeVisitor visitor) {
    visitor.visit(this);
  }

  @Override
  public void addChild(AstNode node) {
    if (node instanceof Expression) {
      setExpression((Expression) node);
    } else {
      super.addChild(node);
    }
  }
}
