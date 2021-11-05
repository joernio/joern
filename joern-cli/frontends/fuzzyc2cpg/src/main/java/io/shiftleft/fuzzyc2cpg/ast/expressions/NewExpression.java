package io.shiftleft.fuzzyc2cpg.ast.expressions;

import io.shiftleft.fuzzyc2cpg.FunctionParser;
import io.shiftleft.fuzzyc2cpg.ast.walking.ASTNodeVisitor;
import io.shiftleft.fuzzyc2cpg.parser.AstNodeFactory;

public class NewExpression extends CallExpressionBase {

  private Identifier targetClass;

  public Identifier getTargetClass() {
    return this.targetClass;
  }

  public void setTargetClass(FunctionParser.Type_nameContext ctx) {
    this.targetClass = new Identifier();
    AstNodeFactory.initializeFromContext(targetClass, ctx);
    super.addChild(targetClass);
  }

  @Override
  public void accept(ASTNodeVisitor visitor) {
    visitor.visit(this);
  }
}
