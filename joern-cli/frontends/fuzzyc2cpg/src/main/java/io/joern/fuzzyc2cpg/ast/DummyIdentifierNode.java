package io.joern.fuzzyc2cpg.ast;

import io.joern.fuzzyc2cpg.ast.expressions.Identifier;

public class DummyIdentifierNode extends Identifier {

  public DummyIdentifierNode() {
    super();
  }

  public String getEscapedCodeStr() {
    return "<unnamed>";
  }
}
