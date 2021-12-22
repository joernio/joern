package io.joern.fuzzyc2cpg.ast.functionDef;

import io.joern.fuzzyc2cpg.ast.AstNode;
import io.joern.fuzzyc2cpg.ast.walking.ASTNodeVisitor;

public abstract class ParameterBase extends AstNode {

    public abstract AstNode getType();

    public abstract String getName();

    @Override
    public void accept(ASTNodeVisitor visitor) {
        visitor.visit(this);
    }
}
