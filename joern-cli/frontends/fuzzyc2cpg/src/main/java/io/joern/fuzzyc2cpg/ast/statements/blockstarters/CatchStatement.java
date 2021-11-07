package io.joern.fuzzyc2cpg.ast.statements.blockstarters;

import io.joern.fuzzyc2cpg.ast.expressions.Identifier;
import io.joern.fuzzyc2cpg.ast.expressions.Variable;
import io.joern.fuzzyc2cpg.ast.logical.statements.BlockStarterWithStmtAndCnd;
import io.joern.fuzzyc2cpg.ast.logical.statements.CompoundStatement;
import io.joern.fuzzyc2cpg.ast.walking.ASTNodeVisitor;

public class CatchStatement extends BlockStarterWithStmtAndCnd {

  private Identifier exceptionIdentifier = null;
  private Variable variable = null;
  private CompoundStatement content = null;

  public Identifier getExceptionIdentifier() {
    return this.exceptionIdentifier;
  }

  public void setExceptionIdentifier(Identifier exceptionIdentifier) {
    this.exceptionIdentifier = exceptionIdentifier;
    super.addChild(exceptionIdentifier);
  }

  public Variable getVariable() {
    return this.variable;
  }

  public void setVariable(Variable variable) {
    this.variable = variable;
    super.addChild(variable);
  }

  public CompoundStatement getContent() {
    return this.content;
  }

  public void setContent(CompoundStatement content) {
    this.content = content;
    super.addChild(content);
  }

  public void accept(ASTNodeVisitor visitor) {
    visitor.visit(this);
  }

}
