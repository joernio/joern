package io.shiftleft.fuzzyc2cpg.ast;

import io.shiftleft.fuzzyc2cpg.ast.walking.ASTNodeVisitor;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;

public class AstNode {

  protected List<AstNode> children;
  protected int childNumber;
  private CodeLocation location = CodeLocation.apply();
  private boolean isInCFG = false;

  // refers to the parsed source code, used for wiring goto labels
  private String code;

  /* constructors */
  public AstNode() {
  }

  public AstNode(AstNode otherNode) {
    copyAttributes(otherNode);
    copyChildren(otherNode);
  }


  /* private helper methods */

  private void copyAttributes(AstNode otherNode) {
    setCodeStr(otherNode.getCodeStr());
    location = otherNode.location;
    setChildNumber(otherNode.childNumber);
    if (otherNode.isInCFG()) {
      markAsCFGNode();
    }
  }

  private void copyChildren(AstNode otherNode) {
    if (otherNode.children != null) {
      for (AstNode n : otherNode.children) {
        addChild(new AstNode(n));
      }
    }
  }


  /* methods for handling children */

  public void addChild(AstNode node) {
    if (children == null) {
      children = new ArrayList<>();
    }
    node.setChildNumber(children.size());
    children.add(node);
  }

  public int getChildCount() {
    if (children == null) {
      return 0;
    }
    return children.size();
  }

  public boolean isLeaf() {
    return (children.size() == 0);
  }

  public AstNode getChild(int i) {
    if (children == null) {
      return null;
    }

    AstNode retval;
    try {
      retval = children.get(i);
    } catch (IndexOutOfBoundsException ex) {
      return null;
    }
    return retval;
  }

  public Iterator<AstNode> getChildIterator() {
    if (children != null) {
      return children.iterator();
    } else {
      return Collections.emptyIterator();
    }

  }

  public AstNode popLastChild() {
    return children.remove(children.size() - 1);
  }


  /* getters and setters */

  public int getChildNumber() {
    return this.childNumber;
  }

  public void setChildNumber(int num) {
    this.childNumber = num;
  }

  public String getEscapedCodeStr() {
    return getCodeStr();
  }

  protected String getCodeStr() {
    return code;
  }

  public void setCodeStr(String aCodeStr) {
    code = aCodeStr;
  }

  public CodeLocation getLocation() {
    return this.location;
  }

  public void setLocation(CodeLocation location) {
    this.location = location;
  }

  public String getTypeAsString() {
    return this.getClass().getSimpleName();
  }


  /* special methods */

  public void accept(ASTNodeVisitor visitor) {
    visitor.visit(this);
  }

  public void markAsCFGNode() {
    isInCFG = true;
  }

  public boolean isInCFG() {
    return isInCFG;
  }


  /* overrides */

  @Override
  public String toString() {
    if (null != getEscapedCodeStr()) {
      return "[" + getEscapedCodeStr() + "]";
    } else {
      return "[" + getTypeAsString() + "]";
    }
  }
}
