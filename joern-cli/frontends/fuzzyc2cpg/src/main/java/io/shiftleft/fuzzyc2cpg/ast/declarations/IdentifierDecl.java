package io.shiftleft.fuzzyc2cpg.ast.declarations;

import io.shiftleft.fuzzyc2cpg.ast.AstNode;
import io.shiftleft.fuzzyc2cpg.ast.expressions.AssignmentExpression;
import io.shiftleft.fuzzyc2cpg.ast.expressions.Identifier;
import io.shiftleft.fuzzyc2cpg.ast.walking.ASTNodeVisitor;

public class IdentifierDecl extends AstNode {

  private IdentifierDeclType type;
  private Identifier name;
  private AssignmentExpression assignment;
  private boolean isTypedef = false;

  public void addChild(AstNode node) {
    if (node instanceof Identifier) {
      if (name == null) {
        setName((Identifier) node);
      }
    } else if (node instanceof IdentifierDeclType) {
      setType((IdentifierDeclType) node);
    } else if (node instanceof AssignmentExpression) {
      setAssignment((AssignmentExpression)node);
    }

    super.addChild(node);
  }

  public Identifier getName() {
    return name;
  }

  private void setName(Identifier name) {
    this.name = name;
  }

  public IdentifierDeclType getType() {
    return type;
  }

  private void setType(IdentifierDeclType type) {
    this.type = type;
  }

  public AssignmentExpression getAssignment() {
    return this.assignment;
  }

  private void setAssignment(AssignmentExpression assignment) {
    this.assignment = assignment;
  }

  public void setIsTypedef(boolean value) {
    isTypedef = value;
  }

  public boolean isTypedef() {
    return isTypedef;
  }

  public void accept(ASTNodeVisitor visitor) {
    visitor.visit(this);
  }
}
