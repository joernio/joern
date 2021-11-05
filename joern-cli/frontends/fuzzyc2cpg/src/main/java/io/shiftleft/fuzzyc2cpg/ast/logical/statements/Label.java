package io.shiftleft.fuzzyc2cpg.ast.logical.statements;

import io.shiftleft.fuzzyc2cpg.ast.AstNode;
import io.shiftleft.fuzzyc2cpg.ast.expressions.StringExpression;
import io.shiftleft.fuzzyc2cpg.ast.walking.ASTNodeVisitor;

public class Label extends Statement {

  private StringExpression name = null;

  public StringExpression getNameChild() {
    return this.name;
  }

  public void setNameChild(StringExpression name) {
    this.name = name;
    super.addChild(name);
  }

  @Override
  public void accept(ASTNodeVisitor visitor) {
    visitor.visit(this);
  }

  public String getLabelName() {
    if (getNameChild() != null) {
      return getNameChild().getEscapedCodeStr();
    }

    String lbl = getEscapedCodeStr().trim();
    return lbl.substring(0, lbl.length()-1);
  }

  @Override
  public void addChild(AstNode node) {
    if (node instanceof StringExpression) {
      setNameChild((StringExpression) node);
    } else {
      super.addChild(node);
    }
  }
}
