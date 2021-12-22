package io.joern.fuzzyc2cpg.ast.expressions;

import io.joern.fuzzyc2cpg.ast.statements.ExpressionHolder;
import io.joern.fuzzyc2cpg.ast.walking.ASTNodeVisitor;

public class Argument extends ExpressionHolder {

    public void accept(ASTNodeVisitor visitor) {
        visitor.visit(this);
    }
}
