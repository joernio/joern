package io.joern.fuzzyc2cpg.ast.logical.statements;

import io.joern.fuzzyc2cpg.ast.AstNode;
import io.joern.fuzzyc2cpg.ast.walking.ASTNodeVisitor;

public class Statement extends AstNode {
    @Override
    public void accept(ASTNodeVisitor visitor) {
        visitor.visit(this);
    }
}
