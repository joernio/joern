package io.shiftleft.fuzzyc2cpg.ast.langc.expressions;

import io.shiftleft.fuzzyc2cpg.ast.expressions.Expression;
import io.shiftleft.fuzzyc2cpg.ast.walking.ASTNodeVisitor;

public class SizeofExpression extends Expression {

  public void accept(ASTNodeVisitor visitor) {
    visitor.visit(this);
  }
}
