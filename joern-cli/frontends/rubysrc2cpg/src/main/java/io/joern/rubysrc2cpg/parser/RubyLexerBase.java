package io.joern.rubysrc2cpg.parser;

import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.Lexer;
import org.antlr.v4.runtime.Token;

/** Aggregates auxiliary features to RubyLexer in a single place. */
public abstract class RubyLexerBase extends Lexer {
    protected RubyLexerBase(CharStream input) {
        super(input);
    }

    /* When encountering '/', we need to decide whether this is a binary operator (e.g. `x / y`) or
     * a regular expression delimiter (e.g. `/(eu|us)/`) occurrence. Our approach is to look at the
     * previously emitted token and decide accordingly.
     */

    /** The previously emitted token (in DEFAULT_CHANNEL.) */
    private Token previousToken = null;

    // Same original behaviour, just updating `previousToken`.
    @Override
    public Token nextToken() {
        Token token = super.nextToken();
        if (token.getChannel() == Token.DEFAULT_CHANNEL){
            previousToken = token;
        }
        return token;
    }

    /** To be invoked when encountering `/`, deciding if it should emit a
     * `REGULAR_EXPRESSION_START` token. */
    protected boolean isStartOfRegex(){
        // When '/' is the first token in the stream.
        if (previousToken == null) {
            return true;
        }

        // When '/' occurs after an operator.
        if (isOperator(previousToken)) {
            return true;
        }

        // Everywhere else: it's the division operator.
        return false;
    }

    /** Is the given token found in the "Operators" category? */
    private boolean isOperator(Token token) {
        switch (token.getType()){
            case RubyLexer.EMARK:
            case RubyLexer.EMARKEQ:
            case RubyLexer.EMARKTILDE:
            case RubyLexer.AMP:
            case RubyLexer.AMP2:
            case RubyLexer.AMPDOT:
            case RubyLexer.BAR:
            case RubyLexer.BAR2:
            case RubyLexer.EQ:
            case RubyLexer.EQ2:
            case RubyLexer.EQ3:
            case RubyLexer.CARET:
            case RubyLexer.LTEQGT:
            case RubyLexer.EQTILDE:
            case RubyLexer.GT:
            case RubyLexer.GTEQ:
            case RubyLexer.LT:
            case RubyLexer.LTEQ:
            case RubyLexer.LT2:
            case RubyLexer.GT2:
            case RubyLexer.PLUS:
            case RubyLexer.MINUS:
            case RubyLexer.STAR:
            case RubyLexer.STAR2:
            case RubyLexer.SLASH:
            case RubyLexer.PERCENT:
            case RubyLexer.TILDE:
            case RubyLexer.PLUSAT:
            case RubyLexer.MINUSAT:
            case RubyLexer.ASSIGNMENT_OPERATOR:
                return true;
            default:
                return false;
        }
    }

    /** To be invoked when in `DEFAULT_MODE`, to check if we are in the context of a string interpolation. */
    protected boolean isInStringInterpolationMode() {
        return _modeStack.size() > 1 && _modeStack.peek() == RubyLexer.DOUBLE_QUOTED_STRING_MODE;
    }

    /** To be invoked when in `DEFAULT_MODE`, to check if we are in the context of a regular expression interpolation. */
    protected boolean isInRegularExpressionInterpolationMode() {
        return _modeStack.size() > 1 && _modeStack.peek() == RubyLexer.REGULAR_EXPRESSION_MODE;
    }

}
