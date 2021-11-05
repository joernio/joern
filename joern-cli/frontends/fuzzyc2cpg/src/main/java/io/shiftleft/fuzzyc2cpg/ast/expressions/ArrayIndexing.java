package io.shiftleft.fuzzyc2cpg.ast.expressions;

import io.shiftleft.fuzzyc2cpg.ast.AstNode;
import io.shiftleft.fuzzyc2cpg.ast.walking.ASTNodeVisitor;

public class ArrayIndexing extends Expression {

  private Expression array = null;
  private Expression index = null;

  public Expression getArrayExpression() {
    return this.array;
  }

  public void setArrayExpression(Expression array) {
    this.array = array;
    super.addChild(array);
  }

  public Expression getIndexExpression() {
    return this.index;
  }

  public void setIndexExpression(Expression index) {
    this.index = index;
    super.addChild(index);
  }

  @Override
  public void addChild(AstNode node) {
    if (node instanceof Expression && getChildCount() == 0) {
      setArrayExpression((Expression) node);
    } else if (node instanceof Expression && getChildCount() == 1) {
      setIndexExpression((Expression) node);
    } else {
      super.addChild(node);
    }
  }

  @Override
  public void accept(ASTNodeVisitor visitor) {
    visitor.visit(this);
  }
}
