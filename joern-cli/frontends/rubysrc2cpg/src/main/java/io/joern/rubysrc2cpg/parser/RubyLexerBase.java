package io.joern.rubysrc2cpg.parser;

import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.Lexer;
import org.antlr.v4.runtime.Token;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;

/** Aggregates auxiliary features to RubyLexer in a single place. */
public abstract class RubyLexerBase extends Lexer {
    protected RubyLexerBase(CharStream input) {
        super(input);
    }

    /* When encountering '/', we need to decide whether this is a binary operator (e.g. `x / y`) or
     * a regular expression delimiter (e.g. `/(eu|us)/`) occurrence. Our approach is to look at the
     * previously emitted token and decide accordingly.
     */
    private final Set<Integer> regexTogglingTokens = new HashSet<>(Arrays.asList(
            // When '/' is the first token in the stream.
            null,
            
            // When '/' occurs after an opening parenthesis, brace or bracket.
            RubyLexer.LPAREN,
            RubyLexer.LCURLY,
            RubyLexer.LBRACK,
            
            // When '/' occurs after a NL.
            RubyLexer.NL,
            
            // When '/' occurs after an operator.
            RubyLexer.EMARK,
            RubyLexer.EMARKEQ,
            RubyLexer.EMARKTILDE,
            RubyLexer.AMP,
            RubyLexer.AMP2,
            RubyLexer.AMPDOT,
            RubyLexer.BAR,
            RubyLexer.BAR2,
            RubyLexer.EQ,
            RubyLexer.EQ2,
            RubyLexer.EQ3,
            RubyLexer.CARET,
            RubyLexer.LTEQGT,
            RubyLexer.EQTILDE,
            RubyLexer.GT,
            RubyLexer.GTEQ,
            RubyLexer.LT,
            RubyLexer.LTEQ,
            RubyLexer.LT2,
            RubyLexer.GT2,
            RubyLexer.PLUS,
            RubyLexer.MINUS,
            RubyLexer.STAR,
            RubyLexer.STAR2,
            RubyLexer.SLASH,
            RubyLexer.PERCENT,
            RubyLexer.TILDE,
            RubyLexer.PLUSAT,
            RubyLexer.MINUSAT,
            RubyLexer.ASSIGNMENT_OPERATOR
    ));

    /** The previously (non-WS) emitted token (in DEFAULT_CHANNEL.) */
    private Token previousNonWsToken = null;

    // Same original behaviour, just updating `previousNonWsToken`.
    @Override
    public Token nextToken() {
        Token token = super.nextToken();
        if (token.getChannel() == Token.DEFAULT_CHANNEL && token.getType() != RubyLexer.WS){
            previousNonWsToken = token;
        }
        return token;
    }

    /** To be invoked when encountering `/`, deciding if it should emit a
     * `REGULAR_EXPRESSION_START` token. */
    protected boolean isStartOfRegex(){
        Integer previousNonWsTokenType = previousNonWsToken == null ? null : previousNonWsToken.getType();
        return regexTogglingTokens.contains(previousNonWsTokenType);
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
