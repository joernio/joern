package io.shiftleft.fuzzyc2cpg.ast.statements;

import io.shiftleft.fuzzyc2cpg.ast.expressions.Expression;
import io.shiftleft.fuzzyc2cpg.ast.walking.ASTNodeVisitor;

// By default, Expressions holding only a single
// child are replaced by their child during
// consolidation. ExpressionHolders are never removed.

public class ExpressionHolder extends Expression {

  @Override
  public String getEscapedCodeStr() {
    Expression expr = getExpression();
    if (expr == null) {
      return "";
    }

    setCodeStr(expr.getEscapedCodeStr());
    return getCodeStr();
  }

  public Expression getExpression() {
    if (children == null) {
      return null;
    }
    return (Expression) children.get(0);
  }

  @Override
  public void accept(ASTNodeVisitor visitor) {
    visitor.visit(this);
  }
}
