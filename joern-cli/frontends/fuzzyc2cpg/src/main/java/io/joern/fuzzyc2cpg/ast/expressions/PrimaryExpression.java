package io.joern.fuzzyc2cpg.ast.expressions;

import io.joern.fuzzyc2cpg.ast.walking.ASTNodeVisitor;

public class PrimaryExpression extends Expression {

  public void accept(ASTNodeVisitor visitor) {
    visitor.visit(this);
  }
}
