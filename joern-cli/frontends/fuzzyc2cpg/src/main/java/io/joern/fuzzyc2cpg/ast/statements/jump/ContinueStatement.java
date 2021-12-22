package io.joern.fuzzyc2cpg.ast.statements.jump;

import io.joern.fuzzyc2cpg.ast.logical.statements.BreakOrContinueStatement;
import io.joern.fuzzyc2cpg.ast.walking.ASTNodeVisitor;

public class ContinueStatement extends BreakOrContinueStatement {

    @Override
    public void accept(ASTNodeVisitor visitor) {
        visitor.visit(this);
    }
}
