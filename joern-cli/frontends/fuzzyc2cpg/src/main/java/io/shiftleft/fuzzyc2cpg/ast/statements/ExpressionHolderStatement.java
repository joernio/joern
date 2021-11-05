package io.shiftleft.fuzzyc2cpg.ast.statements;

import io.shiftleft.fuzzyc2cpg.ast.AstNode;
import io.shiftleft.fuzzyc2cpg.ast.expressions.Expression;
import io.shiftleft.fuzzyc2cpg.ast.logical.statements.Statement;
import io.shiftleft.fuzzyc2cpg.ast.walking.ASTNodeVisitor;

public class ExpressionHolderStatement extends Statement {

  private Expression expression = null;

  public Expression getExpression() {
    return this.expression;
  }

  public void setExpression(Expression expression) {
    this.expression = expression;
    super.addChild(expression);
  }

  @Override
  public String getEscapedCodeStr() {

    Expression expr = getExpression();
    if (expr == null) {
      return "";
    }

    setCodeStr(expr.getEscapedCodeStr());
    return getCodeStr();
  }

  @Override
  public void addChild(AstNode node) {
    if (node instanceof Expression) {
      setExpression((Expression) node);
    } else {
      super.addChild(node);
    }
  }

  @Override
  public void accept(ASTNodeVisitor visitor) {
    visitor.visit(this);
  }
}
