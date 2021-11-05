package io.shiftleft.fuzzyc2cpg.ast.declarations;

import io.shiftleft.fuzzyc2cpg.ast.AstNode;
import io.shiftleft.fuzzyc2cpg.ast.DummyIdentifierNode;
import io.shiftleft.fuzzyc2cpg.ast.expressions.Identifier;
import io.shiftleft.fuzzyc2cpg.ast.functionDef.TemplateParameterList;
import io.shiftleft.fuzzyc2cpg.ast.logical.statements.CompoundStatement;
import io.shiftleft.fuzzyc2cpg.ast.logical.statements.Statement;
import io.shiftleft.fuzzyc2cpg.ast.walking.ASTNodeVisitor;
import java.util.LinkedList;

public class ClassDefStatement extends Statement {

  public Identifier identifier = new DummyIdentifierNode();
  public CompoundStatement content = new CompoundStatement();
  protected TemplateParameterList templateParameterList;
  public LinkedList<Identifier> baseClasses = new LinkedList<>();

  public void addChild(AstNode expression) {
    if (expression instanceof Identifier) {
      setIdentifier((Identifier) expression);
    } else if (expression instanceof TemplateParameterList) {
      setTemplateParameterList((TemplateParameterList) expression);
    } else {
      super.addChild(expression);
    }
  }

  public void addBaseClass(Identifier baseClass){
    this.baseClasses.add(baseClass);
    super.addChild(baseClass);
  }

  public Identifier getIdentifier() {
    return this.identifier;
  }

  private void setIdentifier(Identifier identifier) {
    this.identifier = identifier;
    super.addChild(identifier);
  }

  public TemplateParameterList getTemplateParameterList() {
    return templateParameterList;
  }

  private void setTemplateParameterList(TemplateParameterList templateParameterList) {
    this.templateParameterList = templateParameterList;
    super.addChild(templateParameterList);
  }

  @Override
  public void accept(ASTNodeVisitor visitor) {
    visitor.visit(this);
  }
}
