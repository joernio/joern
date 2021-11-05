package io.shiftleft.fuzzyc2cpg.ast.expressions;

import io.shiftleft.fuzzyc2cpg.FunctionParser;
import io.shiftleft.fuzzyc2cpg.ast.walking.ASTNodeVisitor;
import io.shiftleft.fuzzyc2cpg.parser.AstNodeFactory;

public class DeleteExpression extends CallExpressionBase {
    private Identifier target;

    public Identifier getTarget() {
        return this.target;
    }

    public void setTarget(FunctionParser.IdentifierContext ctx) {
        this.target = new Identifier();
        AstNodeFactory.initializeFromContext(target, ctx);
        super.addChild(target);
    }

    @Override
    public void accept(ASTNodeVisitor visitor) {
        visitor.visit(this);
    }
}
