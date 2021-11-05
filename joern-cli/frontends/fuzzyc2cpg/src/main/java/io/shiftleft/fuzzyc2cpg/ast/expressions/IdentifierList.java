package io.shiftleft.fuzzyc2cpg.ast.expressions;

import io.shiftleft.fuzzyc2cpg.ast.AstNode;
import io.shiftleft.fuzzyc2cpg.ast.walking.ASTNodeVisitor;

import java.util.Iterator;
import java.util.LinkedList;

public class IdentifierList extends AstNode implements Iterable<Identifier> {

  private LinkedList<Identifier> identifiers = new LinkedList<Identifier>();

  public int size() {
    return this.identifiers.size();
  }

  public Identifier getIdentifier(int i) {
    return this.identifiers.get(i);
  }

  public void addIdentifier(Identifier identifier) {
    this.identifiers.add(identifier);
    super.addChild(identifier);
  }

  @Override
  public Iterator<Identifier> iterator() {
    return this.identifiers.iterator();
  }

  @Override
  public void accept(ASTNodeVisitor visitor) {
    visitor.visit(this);
  }
}
