package io.joern.fuzzyc2cpg.ast.langc.expressions;

import io.joern.fuzzyc2cpg.ast.AstNode;
import io.joern.fuzzyc2cpg.ast.expressions.ArgumentList;
import io.joern.fuzzyc2cpg.ast.expressions.CallExpressionBase;
import io.joern.fuzzyc2cpg.ast.expressions.Identifier;
import io.joern.fuzzyc2cpg.ast.walking.ASTNodeVisitor;

public class CallExpression extends CallExpressionBase {

  @Override
  public void addChild(AstNode node) {
    if (node instanceof Identifier) {
      setTargetFunc((Identifier) node);
    } else if (node instanceof ArgumentList) {
      setArgumentList((ArgumentList) node);
    } else {
      super.addChild(node);
    }
  }

  public void accept(ASTNodeVisitor visitor) {
    visitor.visit(this);
  }
}
