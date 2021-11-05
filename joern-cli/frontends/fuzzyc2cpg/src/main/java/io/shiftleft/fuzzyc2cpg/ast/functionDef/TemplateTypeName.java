package io.shiftleft.fuzzyc2cpg.ast.functionDef;

import io.shiftleft.fuzzyc2cpg.ast.AstNode;

public class TemplateTypeName extends AstNode {

  private final String typeName;

  public TemplateTypeName(String typeName) {
    this.typeName = typeName;
  }
}
