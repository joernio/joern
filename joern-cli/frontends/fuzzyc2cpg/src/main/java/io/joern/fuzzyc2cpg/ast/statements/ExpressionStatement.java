package io.joern.fuzzyc2cpg.ast.statements;

import io.joern.fuzzyc2cpg.ast.walking.ASTNodeVisitor;

public class ExpressionStatement extends ExpressionHolderStatement {
    @Override
    public void accept(ASTNodeVisitor visitor) {
        visitor.visit(this);
    }

}
