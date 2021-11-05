package io.shiftleft.fuzzyc2cpg.ast.functionDef;

import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.stream.Collectors;

import io.shiftleft.fuzzyc2cpg.ast.AstNode;

public class TemplateParameterList extends AstNode implements Iterable<TemplateBase>  {
  private final List<TemplateBase> templates = new LinkedList<>();

  private void addTemplateParameter(TemplateBase template) {
    this.templates.add(template);
    super.addChild(template);
  }

  public int size() {
    return templates.size();
  }

  @Override
  public void addChild(AstNode node) {
    if (node instanceof TemplateBase) { addTemplateParameter((TemplateBase) node); }
    else { super.addChild(node); }
  }

  @Override
  public String getEscapedCodeStr() {
    String codeStr = templates
      .stream()
      .map(TemplateBase::getEscapedCodeStr)
      .collect(Collectors.joining(",", "<", ">"));

    setCodeStr(codeStr);

    return codeStr;
  }

  @Override
  public Iterator<TemplateBase> iterator() {
    return templates.iterator();
  }
}
