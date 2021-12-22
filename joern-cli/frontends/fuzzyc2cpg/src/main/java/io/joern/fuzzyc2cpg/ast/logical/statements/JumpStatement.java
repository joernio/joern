package io.joern.fuzzyc2cpg.ast.logical.statements;

import io.joern.fuzzyc2cpg.ast.walking.ASTNodeVisitor;

public class JumpStatement extends Statement {
    @Override
    public void accept(ASTNodeVisitor visitor) {
        visitor.visit(this);
    }

}
