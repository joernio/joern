package io.shiftleft.fuzzyc2cpg.ast;

import org.antlr.v4.runtime.ParserRuleContext;

abstract public class AstNodeBuilder<T extends AstNode> {

  protected T item;

  public T getItem() {
    return item;
  }

  abstract public void createNew(ParserRuleContext ctx);

}
