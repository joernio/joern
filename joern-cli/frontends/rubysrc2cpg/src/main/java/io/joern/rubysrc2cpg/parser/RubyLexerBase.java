package io.joern.rubysrc2cpg.parser;

import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.Lexer;
import org.antlr.v4.runtime.Token;
import static io.joern.rubysrc2cpg.parser.RubyLexer.*;

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
    protected boolean couldBeRegexStart(){
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
            case EMARK:
            case EMARKEQ:
            case EMARKTILDE:
            case AMP:
            case AMP2:
            case AMPDOT:
            case BAR:
            case BAR2:
            case EQ:
            case EQ2:
            case EQ3:
            case CARET:
            case LTEQGT:
            case EQTILDE:
            case GT:
            case GTEQ:
            case LT:
            case LTEQ:
            case LT2:
            case GT2:
            case PLUS:
            case MINUS:
            case STAR:
            case STAR2:
            case SLASH:
            case PERCENT:
            case TILDE:
            case PLUSAT:
            case MINUSAT:
            case ASSIGNMENT_OPERATOR:
                return true;
            default:
                return false;
        }
    }

    /** To be invoked when in `DEFAULT_MODE`, to check if this is part of a string interpolation. */
    protected boolean isInStringInterpolationMode() {
        return _modeStack.size() > 1 && _modeStack.peek() == DOUBLE_QUOTED_STRING_MODE;
    }

    /** To be invoked when in `DEFAULT_MODE`, to check if this is part of a regular expression interpolation. */
    protected boolean isInRegularExpressionInterpolationMode() {
        return _modeStack.size() > 1 && _modeStack.peek() == REGULAR_EXPRESSION_MODE;
    }

}
