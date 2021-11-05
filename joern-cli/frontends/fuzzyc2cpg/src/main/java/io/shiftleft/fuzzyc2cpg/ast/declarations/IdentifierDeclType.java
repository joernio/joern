package io.shiftleft.fuzzyc2cpg.ast.declarations;

import io.shiftleft.fuzzyc2cpg.ast.AstNode;
import io.shiftleft.fuzzyc2cpg.ast.walking.ASTNodeVisitor;

public class IdentifierDeclType extends AstNode {

  public String baseType;
  public String completeType;

  public String getEscapedCodeStr() {
    return completeType;
  }

  @Override
  public void accept(ASTNodeVisitor visitor) {
    visitor.visit(this);
  }
}
