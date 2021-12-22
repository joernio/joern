package io.joern.fuzzyc2cpg.ast.functionDef;

import io.joern.fuzzyc2cpg.ast.AstNode;

public class TemplateTypeName extends AstNode {

    private final String typeName;

    public TemplateTypeName(String typeName) {
        this.typeName = typeName;
    }
}
