package io.shiftleft.fuzzyc2cpg.ast.logical.statements;

import io.shiftleft.fuzzyc2cpg.ast.AstNode;
import io.shiftleft.fuzzyc2cpg.ast.walking.ASTNodeVisitor;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

public class CompoundStatement extends Statement implements Iterable<AstNode> {

  protected static final List<AstNode> emptyList = new LinkedList<AstNode>();

  // TODO would it not be better to expose only the iterator instead?
  public List<AstNode> getStatements() {
    return null == children ? emptyList : children;
  }

  public int size() {
    return getStatements().size();
  }

  public AstNode getStatement(int i) {
    return getStatements().get(i);
  }

  // Note: These may be all kinds of AST nodes: instances of Statement, but also
  // instances of Expression, FunctionDef, or even null nodes.
  public void addStatement(AstNode statement) {
    super.addChild(statement);
  }


  public String getEscapedCodeStr() {
    return "";
  }

  @Override
  public void accept(ASTNodeVisitor visitor) {
    visitor.visit(this);
  }

  @Override
  public Iterator<AstNode> iterator() {
    return getStatements().iterator();
  }
}
