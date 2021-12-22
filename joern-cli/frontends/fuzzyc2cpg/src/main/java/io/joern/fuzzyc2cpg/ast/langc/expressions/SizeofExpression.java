package io.joern.fuzzyc2cpg.ast.langc.expressions;

import io.joern.fuzzyc2cpg.ast.expressions.Expression;
import io.joern.fuzzyc2cpg.ast.walking.ASTNodeVisitor;

public class SizeofExpression extends Expression {

    public void accept(ASTNodeVisitor visitor) {
        visitor.visit(this);
    }
}
