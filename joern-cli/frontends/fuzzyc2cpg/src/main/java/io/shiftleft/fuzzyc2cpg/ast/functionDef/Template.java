package io.shiftleft.fuzzyc2cpg.ast.functionDef;

import io.shiftleft.fuzzyc2cpg.ast.AstNode;
import io.shiftleft.fuzzyc2cpg.ast.walking.ASTNodeVisitor;

public class Template extends TemplateBase {
  private TemplateTypeName typeName;

  private void setTemplateTypeName(TemplateTypeName typeName) {
    this.typeName = typeName;
    super.addChild(typeName);
  }

  @Override
  public String getName() {
    return typeName.getEscapedCodeStr();
  }

  public void addChild(AstNode node) {
    if (node instanceof TemplateTypeName) {
      setTemplateTypeName((TemplateTypeName) node);
    } else {
      super.addChild(node);
    }
  }

  @Override
  public void accept(ASTNodeVisitor visitor) {
    visitor.visit(this);
  }
}
