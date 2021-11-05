package io.shiftleft.fuzzyc2cpg.ast.statements;

import io.shiftleft.fuzzyc2cpg.ast.AstNode;
import io.shiftleft.fuzzyc2cpg.ast.declarations.IdentifierDeclType;
import io.shiftleft.fuzzyc2cpg.ast.logical.statements.Statement;
import io.shiftleft.fuzzyc2cpg.ast.walking.ASTNodeVisitor;
import java.util.List;

public class IdentifierDeclStatement extends Statement {

  private IdentifierDeclType type = new IdentifierDeclType();

  public void addChild(AstNode node) {
    if (node instanceof IdentifierDeclType) {
      setType((IdentifierDeclType) node);
      return; // Do NOT add type to children.
    }
    super.addChild(node);
  }

  public IdentifierDeclType getType() {
    return type;
  }

  private void setType(IdentifierDeclType node) {
    type = node;
  }

  public List<AstNode> getIdentifierDeclList() {
    return children;
  }

  public void accept(ASTNodeVisitor visitor) {
    visitor.visit(this);
  }

}
