package io.joern.fuzzyc2cpg.ast.langc.functiondef;

import io.joern.fuzzyc2cpg.ast.AstNode;
import io.joern.fuzzyc2cpg.ast.expressions.Identifier;
import io.joern.fuzzyc2cpg.ast.functionDef.ParameterBase;
import io.joern.fuzzyc2cpg.ast.walking.ASTNodeVisitor;

public class Parameter extends ParameterBase {

  private ParameterType type = new ParameterType();
  private Identifier identifier = new Identifier();

  public ParameterType getType() {
    return type;
  }

  public void setType(AstNode type) {
    if (type instanceof ParameterType) {
      this.type = (ParameterType) type;
    }
    super.addChild(type);
  }

  @Override
  public String getName() {
    return this.getIdentifier().getEscapedCodeStr();
  }

  public Identifier getIdentifier() {
    return this.identifier;
  }

  private void setIdentifier(Identifier identifier) {
    this.identifier = identifier;
    super.addChild(identifier);
  }

  public void addChild(AstNode node) {
    if (node instanceof ParameterType) {
      setType((ParameterType) node);
    } else if (node instanceof Identifier) {
      setIdentifier((Identifier) node);
    } else {
      super.addChild(node);
    }
  }

  @Override
  public void accept(ASTNodeVisitor visitor) {
    visitor.visit(this);
  }
}
