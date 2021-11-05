package io.shiftleft.fuzzyc2cpg.parser.functions.builder;

import org.antlr.v4.runtime.ParserRuleContext;

import io.shiftleft.fuzzyc2cpg.ModuleParser.Template_nameContext;
import io.shiftleft.fuzzyc2cpg.ast.AstNodeBuilder;
import io.shiftleft.fuzzyc2cpg.ast.functionDef.Template;
import io.shiftleft.fuzzyc2cpg.ast.functionDef.TemplateParameterList;
import io.shiftleft.fuzzyc2cpg.parser.AstNodeFactory;

public class TemplateParameterListBuilder extends AstNodeBuilder<TemplateParameterList> {
  private final TemplateParameterList templateParameterList = new TemplateParameterList();

  public TemplateParameterListBuilder() {
    item = templateParameterList;
  }

  @Override
  public void createNew(ParserRuleContext ctx) {
    AstNodeFactory.initializeFromContext(templateParameterList, ctx);
  }

  public void addTemplateParameter(Template_nameContext ctx) {
    Template template = AstNodeFactory.create(ctx);
    templateParameterList.addChild(template);
  }

}
