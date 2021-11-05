package io.shiftleft.fuzzyc2cpg.ast.statements.blockstarters;

import io.shiftleft.fuzzyc2cpg.ast.expressions.StringExpression;
import io.shiftleft.fuzzyc2cpg.ast.logical.statements.BlockStarter;
import io.shiftleft.fuzzyc2cpg.ast.logical.statements.CompoundStatement;
import io.shiftleft.fuzzyc2cpg.ast.walking.ASTNodeVisitor;

public class NamespaceStatement extends BlockStarter {

  private StringExpression name = null;
  private CompoundStatement content = null;

  public StringExpression getName() {
    return this.name;
  }

  public void setName(StringExpression name) {
    this.name = name;
    super.addChild(name);
  }

  public CompoundStatement getContent() {
    return this.content;
  }

  public void setContent(CompoundStatement content) {
    this.content = content;
    super.addChild(content);
  }

  @Override
  public void accept(ASTNodeVisitor visitor) {
    visitor.visit(this);
  }
}
