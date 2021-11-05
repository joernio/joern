package io.shiftleft.fuzzyc2cpg.ast.functionDef;

import io.shiftleft.fuzzyc2cpg.ast.AstNode;
import io.shiftleft.fuzzyc2cpg.ast.walking.ASTNodeVisitor;
import java.util.Iterator;
import java.util.LinkedList;

public class ParameterList extends AstNode implements Iterable<ParameterBase> {

  private LinkedList<ParameterBase> parameters = new LinkedList<ParameterBase>();

  public void addChild(AstNode node) {
    if (node instanceof ParameterBase) {
      addParameter((ParameterBase) node);
    } else {
      super.addChild(node);
    }
  }

  public int size() {
    return this.parameters.size();
  }

  public ParameterBase getParameter(int i) {
    return this.parameters.get(i);
  }

  public void addParameter(ParameterBase parameter) {
    this.parameters.add(parameter);
    super.addChild(parameter);
  }

  @Override
  public String getEscapedCodeStr() {
    return getEscapedCodeStr(true);
  }

  public String getEscapedCodeStr(boolean includeParameterName) {
    if (parameters.size() == 0) {
      setCodeStr("");
      return getCodeStr();
    }

    StringBuilder s = new StringBuilder();

    parameters.iterator().forEachRemaining(param -> {
      if (includeParameterName) s.append(param.getEscapedCodeStr()).append(",");
      else s.append(param.getType().getEscapedCodeStr()).append(",");
    });

    setCodeStr(s.substring(0, s.length() - 1));

    return getCodeStr();
  }

  @Override
  public void accept(ASTNodeVisitor visitor) {
    visitor.visit(this);
  }

  @Override
  public Iterator<ParameterBase> iterator() {
    return this.parameters.iterator();
  }
}
