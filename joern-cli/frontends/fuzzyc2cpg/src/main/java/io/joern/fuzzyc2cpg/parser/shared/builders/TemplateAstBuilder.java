package io.joern.fuzzyc2cpg.parser.shared.builders;

import io.joern.fuzzyc2cpg.ModuleParser;
import io.joern.fuzzyc2cpg.ast.AstNode;
import io.joern.fuzzyc2cpg.ast.AstNodeBuilder;
import io.joern.fuzzyc2cpg.parser.functions.builder.TemplateParameterListBuilder;

public abstract class TemplateAstBuilder<T extends AstNode> extends AstNodeBuilder<T> {

    protected final TemplateParameterListBuilder templateParamBuilder = new TemplateParameterListBuilder();

    public void setTemplateList(ModuleParser.Template_declContext ctx) {
        templateParamBuilder.createNew(ctx);
        item.addChild(templateParamBuilder.getItem());
    }

    public void addTemplateParameter(ModuleParser.Template_nameContext ctx) {
        templateParamBuilder.addTemplateParameter(ctx);
    }
}
