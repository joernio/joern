lexer grammar DeprecatedRubyLexer;

// --------------------------------------------------------
// Auxiliary tokens and features
// --------------------------------------------------------

@header {
    package io.joern.rubysrc2cpg.deprecated.parser;
}

tokens {
    STRING_INTERPOLATION_END,
    REGULAR_EXPRESSION_INTERPOLATION_END,
    REGULAR_EXPRESSION_START,
    QUOTED_NON_EXPANDED_STRING_LITERAL_END,
    QUOTED_NON_EXPANDED_STRING_ARRAY_LITERAL_END,
    QUOTED_NON_EXPANDED_SYMBOL_ARRAY_LITERAL_END,
    QUOTED_EXPANDED_REGULAR_EXPRESSION_END,
    QUOTED_EXPANDED_STRING_LITERAL_END,
    QUOTED_EXPANDED_EXTERNAL_COMMAND_LITERAL_END,
    QUOTED_EXPANDED_STRING_ARRAY_LITERAL_END,
    QUOTED_EXPANDED_SYMBOL_ARRAY_LITERAL_END,
    DELIMITED_STRING_INTERPOLATION_END,
    DELIMITED_ARRAY_ITEM_INTERPOLATION_END,
    
    // The following tokens are created by `RubyLexerPostProcessor` only.
    NON_EXPANDED_LITERAL_CHARACTER_SEQUENCE,
    EXPANDED_LITERAL_CHARACTER_SEQUENCE
}

options {
    superClass = DeprecatedRubyLexerBase;
}

// --------------------------------------------------------
// Keywords
// --------------------------------------------------------

LINE__:'__LINE__';
ENCODING__: '__ENCODING__';
FILE__: '__FILE__';
BEGIN_: 'BEGIN';
END_: 'END';
ALIAS: 'alias';
AND: 'and';
BEGIN: 'begin';
BREAK: 'break';
CASE: 'case';
CLASS: 'class';
DEF: 'def';
IS_DEFINED: 'defined?';
DO: 'do';
ELSE: 'else';
ELSIF: 'elsif';
END: 'end';
ENSURE: 'ensure';
FOR: 'for';
FALSE: 'false';
IF: 'if';
IN: 'in';
MODULE: 'module';
NEXT: 'next';
NIL: 'nil';
NOT: 'not';
OR: 'or';
REDO: 'redo';
RESCUE: 'rescue';
RETRY: 'retry';
RETURN: 'return';
SELF: 'self';
SUPER: 'super';
THEN: 'then';
TRUE: 'true';
UNDEF: 'undef';
UNLESS: 'unless';
UNTIL: 'until';
WHEN: 'when';
WHILE: 'while';
YIELD: 'yield';

fragment KEYWORD
    :   LINE__
    |   ENCODING__
    |   FILE__
    |   BEGIN_
    |   END_
    |   ALIAS
    |   AND
    |   BEGIN
    |   BREAK
    |   CASE
    |   CLASS
    |   DEF
    |   IS_DEFINED
    |   DO
    |   ELSE
    |   ELSIF
    |   END
    |   ENSURE
    |   FOR
    |   FALSE
    |   IF
    |   IN
    |   MODULE
    |   NEXT
    |   NIL
    |   NOT
    |   OR
    |   REDO
    |   RESCUE
    |   RETRY
    |   RETURN
    |   SELF
    |   SUPER
    |   THEN
    |   TRUE
    |   UNDEF
    |   UNLESS
    |   UNTIL
    |   WHEN
    |   WHILE
    |   YIELD
    ;

// --------------------------------------------------------
// Punctuators
// --------------------------------------------------------

LBRACK: '[';
RBRACK: ']';
LPAREN: '(';
RPAREN: ')';
LCURLY: '{';
RCURLY: '}'
    {
        if (isEndOfInterpolation()) {
            popMode();
            setType(popInterpolationEndTokenType());
        }
    }
;
COLON: ':';
COLON2: '::';
COMMA: ',';
SEMI: ';';
DOT: '.';
DOT2: '..';
DOT3: '...';
QMARK: '?';
EQGT: '=>';
MINUSGT: '->';

fragment PUNCTUATOR
    :   LBRACK
    |   RBRACK
    |   LPAREN
    |   RPAREN
    |   LCURLY
    |   RCURLY
    |   COLON2
    |   COMMA
    |   SEMI
    |   DOT2
    |   DOT3
    |   QMARK
    |   COLON
    |   EQGT
    ;

// --------------------------------------------------------
// Operators
// --------------------------------------------------------

EMARK: '!';
EMARKEQ: '!=';
EMARKTILDE: '!~';
AMP: '&';
AMP2: '&&';
AMPDOT: '&.';
BAR: '|';
BAR2: '||';
EQ: '=';
EQ2: '==';
EQ3: '===';
CARET: '^';
LTEQGT: '<=>';
EQTILDE: '=~';
GT: '>';
GTEQ: '>=';
LT: '<';
LTEQ: '<=';
LT2: '<<';
GT2: '>>';
PLUS: '+';
MINUS: '-';
STAR: '*';
STAR2: '**';
SLASH: '/'
    {
        if (isStartOfRegexLiteral()) {
            setType(REGULAR_EXPRESSION_START);
            pushMode(REGULAR_EXPRESSION_MODE);
        }
    }
;
PERCENT: '%';
TILDE: '~';
// These tokens should only occur after a DEF token, as they are solely used to (re)define unary + and - operators.
// This way we won't emit the wrong token in e.g. `x+@y` (which means + between x and @y)
PLUSAT: '+@'  {previousNonWsTokenTypeOrEOF() == DEF}?;
MINUSAT: '-@' {previousNonWsTokenTypeOrEOF() == DEF}?;

ASSIGNMENT_OPERATOR
    :   ASSIGNMENT_OPERATOR_NAME '='
    ;

fragment ASSIGNMENT_OPERATOR_NAME
    :   AMP
    |   AMP2
    |   BAR
    |   BAR2
    |   CARET
    |   LT2
    |   GT2
    |   PLUS
    |   MINUS
    |   STAR
    |   STAR2
    |   PERCENT
    |   SLASH
    ;

fragment OPERATOR_METHOD_NAME
    :   CARET
    |   AMP
    |   BAR
    |   LTEQGT
    |   EQ2
    |   EQ3
    |   EQTILDE
    |   GT
    |   GTEQ
    |   LT
    |   LTEQ
    |   LT2
    |   GT2
    |   PLUS
    |   MINUS
    |   STAR
    |   SLASH
    |   PERCENT
    |   STAR2
    |   TILDE
    |   PLUSAT
    |   MINUSAT
    |   '[]'
    |   '[]='
    ;

// --------------------------------------------------------
// String literals
// --------------------------------------------------------

SINGLE_QUOTED_STRING_LITERAL
    :   '\'' SINGLE_QUOTED_STRING_CHARACTER*? '\''
    ;

fragment SINGLE_QUOTED_STRING_CHARACTER
    :   SINGLE_QUOTED_STRING_NON_ESCAPED_CHARACTER
    |   SINGLE_QUOTED_ESCAPE_SEQUENCE
    ;

fragment SINGLE_QUOTED_STRING_NON_ESCAPED_CHARACTER
    :   ~['\\]
    ;

fragment SINGLE_QUOTED_ESCAPE_SEQUENCE
    :   SINGLE_ESCAPE_CHARACTER_SEQUENCE
    |   SINGLE_QUOTED_STRING_NON_ESCAPED_CHARACTER_SEQUENCE
    ;

fragment SINGLE_ESCAPE_CHARACTER_SEQUENCE
    :   '\\' SINGLE_QUOTED_STRING_META_CHARACTER
    ;

fragment SINGLE_QUOTED_STRING_META_CHARACTER
    :   ['\\]
    ;

fragment SINGLE_QUOTED_STRING_NON_ESCAPED_CHARACTER_SEQUENCE
    :   '\\' SINGLE_QUOTED_STRING_NON_ESCAPED_CHARACTER
    ;

DOUBLE_QUOTED_STRING_START
    :   '"'
        -> pushMode(DOUBLE_QUOTED_STRING_MODE)
    ;

QUOTED_NON_EXPANDED_STRING_LITERAL_START
    :   '%q' {!Character.isAlphabetic(_input.LA(1))}?
    {
        pushQuotedDelimiter(_input.LA(1));
        pushQuotedEndTokenType(QUOTED_NON_EXPANDED_STRING_LITERAL_END);
        _input.consume();
    }
        -> pushMode(NON_EXPANDED_DELIMITED_STRING_MODE)
    ;
    
QUOTED_EXPANDED_STRING_LITERAL_START
    :   '%Q' {!Character.isAlphabetic(_input.LA(1))}?
    {
        pushQuotedDelimiter(_input.LA(1));
        pushQuotedEndTokenType(QUOTED_EXPANDED_STRING_LITERAL_END);
        _input.consume();
        pushMode(EXPANDED_DELIMITED_STRING_MODE);
    }
    //  This check exists to prevent issuing a QUOTED_EXPANDED_STRING_LITERAL_START
    //  in obvious arithmetic expressions, such as `20 %(x+1)`.
    //  Note, however, that we can't have a perfect test at this stage. For instance,
    //  in `x = 1; x %(2)`, it's clear that's an arithmetic expression, but we
    //  will still emit a QUOTED_EXPANDED_STRING_LITERAL_START.
    |   '%(' {!isNumericTokenType(previousTokenTypeOrEOF())}?
    {
        pushQuotedDelimiter('(');
        pushQuotedEndTokenType(QUOTED_EXPANDED_STRING_LITERAL_END);
        pushMode(EXPANDED_DELIMITED_STRING_MODE);
    }
    ;

QUOTED_EXPANDED_REGULAR_EXPRESSION_START
    :   '%r' {!Character.isAlphabetic(_input.LA(1))}?
    {
        pushQuotedDelimiter(_input.LA(1));
        pushQuotedEndTokenType(QUOTED_EXPANDED_REGULAR_EXPRESSION_END);
        _input.consume();
    }
        -> pushMode(EXPANDED_DELIMITED_STRING_MODE)
    ;
    
QUOTED_EXPANDED_EXTERNAL_COMMAND_LITERAL_START
    :   '%x' {!Character.isAlphabetic(_input.LA(1))}?
    {
        pushQuotedDelimiter(_input.LA(1));
        pushQuotedEndTokenType(QUOTED_EXPANDED_EXTERNAL_COMMAND_LITERAL_END);
        _input.consume();
    }
        -> pushMode(EXPANDED_DELIMITED_STRING_MODE)
    ;

// --------------------------------------------------------
// String (Word) array literals
// --------------------------------------------------------

QUOTED_NON_EXPANDED_STRING_ARRAY_LITERAL_START
    :   '%w' {!Character.isAlphabetic(_input.LA(1))}?
    {
        pushQuotedDelimiter(_input.LA(1));
        pushQuotedEndTokenType(QUOTED_NON_EXPANDED_STRING_ARRAY_LITERAL_END);
        _input.consume();
    }
        -> pushMode(NON_EXPANDED_DELIMITED_ARRAY_MODE)
    ;
    
QUOTED_EXPANDED_STRING_ARRAY_LITERAL_START
    :   '%W' {!Character.isAlphabetic(_input.LA(1))}?
    {
        pushQuotedDelimiter(_input.LA(1));
        pushQuotedEndTokenType(QUOTED_EXPANDED_STRING_ARRAY_LITERAL_END);
        _input.consume();
    }
        -> pushMode(EXPANDED_DELIMITED_ARRAY_MODE)
    ;

// --------------------------------------------------------
// Here doc literals
// --------------------------------------------------------

HERE_DOC_IDENTIFIER
 : '<<' [-~]? [\t]* IDENTIFIER
 ;

HERE_DOC
 : '<<' [-~]? [\t]* IDENTIFIER [a-zA-Z_0-9]* NL ( {!heredocEndAhead(getText())}? . )* [a-zA-Z_] [a-zA-Z_0-9]*
 ;

// --------------------------------------------------------
// Symbol array literals
// --------------------------------------------------------

QUOTED_NON_EXPANDED_SYMBOL_ARRAY_LITERAL_START
    :   '%i' {!Character.isAlphabetic(_input.LA(1))}?
    {
        pushQuotedDelimiter(_input.LA(1));
        pushQuotedEndTokenType(QUOTED_NON_EXPANDED_SYMBOL_ARRAY_LITERAL_END);
        _input.consume();
    }
        -> pushMode(NON_EXPANDED_DELIMITED_ARRAY_MODE)
    ;

QUOTED_EXPANDED_SYMBOL_ARRAY_LITERAL_START
    :   '%I' {!Character.isAlphabetic(_input.LA(1))}?
    {
        pushQuotedDelimiter(_input.LA(1));
        pushQuotedEndTokenType(QUOTED_EXPANDED_SYMBOL_ARRAY_LITERAL_END);
        _input.consume();
    }
        -> pushMode(EXPANDED_DELIMITED_ARRAY_MODE)
    ;

// --------------------------------------------------------
// Data section
// --------------------------------------------------------

END_OF_PROGRAM_MARKER
    :   '__END__' {getCharPositionInLine() == 7}? '\r'? '\n'
        -> pushMode(DATA_SECTION_MODE), skip
    ;

// --------------------------------------------------------
// Numeric literals
// --------------------------------------------------------

DECIMAL_INTEGER_LITERAL
    :   UNPREFIXED_DECIMAL_INTEGER_LITERAL
    |   PREFIXED_DECIMAL_INTEGER_LITERAL
    ;

BINARY_INTEGER_LITERAL
    :   '0' [bB] BINARY_DIGIT ('_'? BINARY_DIGIT)*
    ;

OCTAL_INTEGER_LITERAL
    :   '0' [_oO]? OCTAL_DIGIT ('_'? OCTAL_DIGIT)*
    ;

HEXADECIMAL_INTEGER_LITERAL
    :   '0' [xX] HEXADECIMAL_DIGIT ('_'? HEXADECIMAL_DIGIT)*
    ;

FLOAT_LITERAL_WITHOUT_EXPONENT
    :   UNPREFIXED_DECIMAL_INTEGER_LITERAL '.' DIGIT_DECIMAL_PART
    ;

FLOAT_LITERAL_WITH_EXPONENT
    :   SIGNIFICAND_PART EXPONENT_PART
    ;

fragment UNPREFIXED_DECIMAL_INTEGER_LITERAL
    :   '0'
    |   DECIMAL_DIGIT_EXCEPT_0 ('_'? DECIMAL_DIGIT)*
    ;

fragment PREFIXED_DECIMAL_INTEGER_LITERAL
    :   '0' [dD] DIGIT_DECIMAL_PART
    ;

fragment SIGNIFICAND_PART
    :   FLOAT_LITERAL_WITHOUT_EXPONENT
    |   UNPREFIXED_DECIMAL_INTEGER_LITERAL
    ;

fragment EXPONENT_PART
    :   [eE] ('+' | '-')? DIGIT_DECIMAL_PART
    ;

fragment BINARY_DIGIT
    :   [0-1]
    ;

fragment OCTAL_DIGIT
    :   [0-7]
    ;

fragment DIGIT_DECIMAL_PART
    :   DECIMAL_DIGIT ('_'? DECIMAL_DIGIT)*
    ;

fragment DECIMAL_DIGIT
    :   [0-9]
    ;

fragment DECIMAL_DIGIT_EXCEPT_0
    :   [1-9]
    ;

fragment HEXADECIMAL_DIGIT
    :   DECIMAL_DIGIT
    |   [a-f]
    |   [A-F]
    ;

// --------------------------------------------------------
// Whitespaces
// --------------------------------------------------------

NL: LINE_TERMINATOR+;
WS: WHITESPACE+;

fragment WHITESPACE
    :   [\u0009]
    |   [\u000b]
    |   [\u000c]
    |   [\u000d]
    |   [\u0020]
    |   LINE_TERMINATOR_ESCAPE_SEQUENCE
    ;

fragment LINE_TERMINATOR_ESCAPE_SEQUENCE
    :   '\\' LINE_TERMINATOR
    ;

fragment LINE_TERMINATOR
    :   '\r'? '\n'
    ;

// --------------------------------------------------------
// Symbols
// --------------------------------------------------------

SYMBOL_LITERAL
    :   ':' (SYMBOL_NAME | (CONSTANT_IDENTIFIER | LOCAL_VARIABLE_IDENTIFIER) '=')
    // This check exists to prevent issuing a SYMBOL_LITERAL in whitespace-free associations, e.g. 
    //      in `foo(x:y)`, so that `:y` is not a SYMBOL_LITERAL
    // or   in `{:x=>1}`, so that `:x=` is not a SYMBOL_LITERAL
    {previousTokenTypeOrEOF() != LOCAL_VARIABLE_IDENTIFIER && _input.LA(1) != '>'}?
    ;

fragment SYMBOL_NAME
    :   INSTANCE_VARIABLE_IDENTIFIER
    |   GLOBAL_VARIABLE_IDENTIFIER
    |   CLASS_VARIABLE_IDENTIFIER
    |   CONSTANT_IDENTIFIER
    |   LOCAL_VARIABLE_IDENTIFIER
    |   METHOD_ONLY_IDENTIFIER
    |   OPERATOR_METHOD_NAME
    |   KEYWORD
    // NOTE: Even though we have PLUSAT and MINUSAT in OPERATOR_METHOD_NAME, the former
    // are not emitted unless there's a DEF token before them, cf. their predicate.
    // Thus, we need to add them explicitly here in order to recognize standalone SYMBOL_LITERAL tokens as well.
    |   '+@'
    |   '-@'
    ;

// --------------------------------------------------------
// Identifiers
// --------------------------------------------------------

LOCAL_VARIABLE_IDENTIFIER
    :   (LOWERCASE_CHARACTER | '_') IDENTIFIER_CHARACTER*
    ;

GLOBAL_VARIABLE_IDENTIFIER
    :   '$' IDENTIFIER_START_CHARACTER IDENTIFIER_CHARACTER*
    |   '$' [0-9]+
    ;

INSTANCE_VARIABLE_IDENTIFIER
    :   '@' IDENTIFIER_START_CHARACTER IDENTIFIER_CHARACTER*
    ;

CLASS_VARIABLE_IDENTIFIER
    :   '@@' IDENTIFIER_START_CHARACTER IDENTIFIER_CHARACTER*
    ;

CONSTANT_IDENTIFIER
    :   UPPERCASE_CHARACTER IDENTIFIER_CHARACTER*
    ;

fragment METHOD_ONLY_IDENTIFIER
    :   (CONSTANT_IDENTIFIER | LOCAL_VARIABLE_IDENTIFIER) ('!' | '?')
    ;


// Similarly to PLUSAT/MINUSAT, this should only occur after a DEF token.
// Otherwise, the assignment `x=nil` would be parsed as (ASSIGNMENT_LIKE_METHOD_IDENTIFIER, NIL)
// instead of the more appropriate (LOCAL_VARIABLE_IDENTIFIER, EQ, NIL).
ASSIGNMENT_LIKE_METHOD_IDENTIFIER
    :   (CONSTANT_IDENTIFIER | LOCAL_VARIABLE_IDENTIFIER) '=' {previousNonWsTokenTypeOrEOF() == DEF}?
    ;

fragment IDENTIFIER_CHARACTER
    :   IDENTIFIER_START_CHARACTER
    |   DECIMAL_DIGIT
    ;

fragment IDENTIFIER_START_CHARACTER
    :   LOWERCASE_CHARACTER
    |   UPPERCASE_CHARACTER
    |   '_'
    ;

fragment LOWERCASE_CHARACTER
    :   [a-z]
    ;

fragment UPPERCASE_CHARACTER
    :   [A-Z]
    ;

fragment IDENTIFIER
    :   LOCAL_VARIABLE_IDENTIFIER
    |   GLOBAL_VARIABLE_IDENTIFIER
    |   CLASS_VARIABLE_IDENTIFIER
    |   INSTANCE_VARIABLE_IDENTIFIER
    |   CONSTANT_IDENTIFIER
    |   METHOD_ONLY_IDENTIFIER
    |   ASSIGNMENT_LIKE_METHOD_IDENTIFIER
    ;

// --------------------------------------------------------
// Comments (are skipped)
// --------------------------------------------------------

SINGLE_LINE_COMMENT
    :   '#' COMMENT_CONTENT?
    -> skip;

MULTI_LINE_COMMENT
    :   MULTI_LINE_COMMENT_BEGIN_LINE .*? MULTI_LINE_COMMENT_END_LINE
    -> skip;

fragment COMMENT_CONTENT
    :   (~[\r\n])+  // Meaning (~LINE_TERMINATOR)+
    ;

fragment MULTI_LINE_COMMENT_BEGIN_LINE
    :   '=begin' {getCharPositionInLine() == 6}? REST_OF_BEGIN_END_LINE? LINE_TERMINATOR
    ;

fragment MULTI_LINE_COMMENT_END_LINE
    :   '=end' {getCharPositionInLine() == 4}? REST_OF_BEGIN_END_LINE? (LINE_TERMINATOR | EOF)
    ;

fragment REST_OF_BEGIN_END_LINE
    :   WHITESPACE+ COMMENT_CONTENT
    ;

// --------------------------------------------------------
// Unrecognized characters
// --------------------------------------------------------

// Any other character shall still be recognized so that the
// recovery mechanism in `io.joern.rubysrc2cpg.astcreation.AntlrParser`
// also handles them. Otherwise, the lexer would complain, not emit
// and the recovery mechanism would not be able to act.

// Note: this must be the very last rule in this lexer specification, as
// otherwise this token would take precedence over any token defined after.
UNRECOGNIZED
    :   .
    ;

// --------------------------------------------------------
// Double quoted string mode
// --------------------------------------------------------

mode DOUBLE_QUOTED_STRING_MODE;

DOUBLE_QUOTED_STRING_END
    :   '"'
        -> popMode
    ;

DOUBLE_QUOTED_STRING_CHARACTER_SEQUENCE
    :   DOUBLE_QUOTED_STRING_CHARACTER+
    ;

fragment INTERPOLATED_CHARACTER_SEQUENCE_FRAGMENT
    :   '#' GLOBAL_VARIABLE_IDENTIFIER
    |   '#' CLASS_VARIABLE_IDENTIFIER
    |   '#' INSTANCE_VARIABLE_IDENTIFIER
    ;

INTERPOLATED_CHARACTER_SEQUENCE
    :   INTERPOLATED_CHARACTER_SEQUENCE_FRAGMENT
    ;

STRING_INTERPOLATION_BEGIN
    :   '#{' 
    {
        pushInterpolationEndTokenType(STRING_INTERPOLATION_END);
        pushMode(DEFAULT_MODE);
    }
    ;

fragment DOUBLE_QUOTED_STRING_CHARACTER
    :   ~["#\\]
    |   '#' {_input.LA(1) != '$' && _input.LA(1) != '@' && _input.LA(1) != '{'}?
    |   DOUBLE_ESCAPE_SEQUENCE
    ;

fragment DOUBLE_ESCAPE_SEQUENCE
    :   SIMPLE_ESCAPE_SEQUENCE
    |   NON_ESCAPED_SEQUENCE
    |   LINE_TERMINATOR_ESCAPE_SEQUENCE
    |   OCTAL_ESCAPE_SEQUENCE
    |   HEXADECIMAL_ESCAPE_SEQUENCE
    |   CONTROL_ESCAPE_SEQUENCE
    ;

fragment CONTROL_ESCAPE_SEQUENCE
    :   '\\' ('C-' | 'c') CONTROL_ESCAPED_CHARACTER
    ;

fragment CONTROL_ESCAPED_CHARACTER
    :   DOUBLE_ESCAPE_SEQUENCE
    |   '?'
    |   ~[\\?]
    ;

fragment OCTAL_ESCAPE_SEQUENCE
    :   '\\' OCTAL_DIGIT OCTAL_DIGIT? OCTAL_DIGIT?
    ;

fragment HEXADECIMAL_ESCAPE_SEQUENCE
    :   '\\x' HEXADECIMAL_DIGIT HEXADECIMAL_DIGIT?
    ;

fragment NON_ESCAPED_SEQUENCE
    :   '\\' NON_ESCAPED_DOUBLE_QUOTED_STRING_CHARACTER
    ;

fragment NON_ESCAPED_DOUBLE_QUOTED_STRING_CHARACTER
    :   ~[\r\nA-Za-z0-9]
    ;

fragment SIMPLE_ESCAPE_SEQUENCE
    :   '\\' DOUBLE_ESCAPED_CHARACTER
    ;

fragment DOUBLE_ESCAPED_CHARACTER
    :   [ntrfvaebsu]
    ;

// --------------------------------------------------------
// Expanded delimited string mode
// --------------------------------------------------------

mode EXPANDED_DELIMITED_STRING_MODE;

DELIMITED_STRING_INTERPOLATION_BEGIN
    :   '#{' 
    {
        pushInterpolationEndTokenType(DELIMITED_STRING_INTERPOLATION_END);
        pushMode(DEFAULT_MODE);
    }
    ;

EXPANDED_VARIABLE_CHARACTER_SEQUENCE
    :   INTERPOLATED_CHARACTER_SEQUENCE_FRAGMENT
    ;

EXPANDED_LITERAL_CHARACTER
    :   NON_EXPANDED_LITERAL_ESCAPE_SEQUENCE
    |   NON_ESCAPED_LITERAL_CHARACTER
    {
        consumeQuotedCharAndMaybePopMode(_input.LA(-1));
    }
    ;

// --------------------------------------------------------
// Non-expanded delimited string mode
// --------------------------------------------------------

mode NON_EXPANDED_DELIMITED_STRING_MODE;


fragment NON_EXPANDED_LITERAL_ESCAPE_SEQUENCE
    :   '\\' NON_ESCAPED_LITERAL_CHARACTER
    ;

fragment NON_ESCAPED_LITERAL_CHARACTER
    :   ~[\r\n]
    |   '\n' {_input.LA(1) != '\r'}?
    ;

NON_EXPANDED_LITERAL_CHARACTER
    :   NON_EXPANDED_LITERAL_ESCAPE_SEQUENCE
    |   NON_ESCAPED_LITERAL_CHARACTER
    {
        consumeQuotedCharAndMaybePopMode(_input.LA(-1));
    }
    ;

// --------------------------------------------------------
// Expanded delimited array mode
// --------------------------------------------------------

mode EXPANDED_DELIMITED_ARRAY_MODE;

DELIMITED_ARRAY_ITEM_INTERPOLATION_BEGIN
    :   '#{'
    {
        pushInterpolationEndTokenType(DELIMITED_ARRAY_ITEM_INTERPOLATION_END);
        pushMode(DEFAULT_MODE);
    }
    ;

EXPANDED_ARRAY_ITEM_SEPARATOR
    :   NON_EXPANDED_ARRAY_ITEM_DELIMITER
    ;

EXPANDED_ARRAY_ITEM_CHARACTER
    :   NON_EXPANDED_LITERAL_ESCAPE_SEQUENCE
    |   NON_ESCAPED_LITERAL_CHARACTER
    {
        consumeQuotedCharAndMaybePopMode(_input.LA(-1));
    }
    ;

// --------------------------------------------------------
// Non-expanded delimited array mode
// --------------------------------------------------------

mode NON_EXPANDED_DELIMITED_ARRAY_MODE;

fragment NON_EXPANDED_ARRAY_ITEM_DELIMITER
    :   [\u0009]
    |   [\u000a]
    |   [\u000b]
    |   [\u000c]
    |   [\u000d]
    |   [\u0020]
    |   '\\' ('\r'? '\n')
    ;

NON_EXPANDED_ARRAY_ITEM_SEPARATOR
    :   NON_EXPANDED_ARRAY_ITEM_DELIMITER
    ;

NON_EXPANDED_ARRAY_ITEM_CHARACTER
    :   NON_EXPANDED_LITERAL_ESCAPE_SEQUENCE
    |   NON_ESCAPED_LITERAL_CHARACTER
    {
        consumeQuotedCharAndMaybePopMode(_input.LA(-1));
    }
    ;

// --------------------------------------------------------
// Regex literal mode
// --------------------------------------------------------

mode REGULAR_EXPRESSION_MODE;

REGULAR_EXPRESSION_END
    :   '/' REGULAR_EXPRESSION_OPTION*
        -> popMode
    ;

REGULAR_EXPRESSION_BODY
    :   REGULAR_EXPRESSION_CHARACTER+
    ;

REGULAR_EXPRESSION_INTERPOLATION_BEGIN
    :   '#{' 
    {
        pushInterpolationEndTokenType(REGULAR_EXPRESSION_INTERPOLATION_END);
        pushMode(DEFAULT_MODE);
    }
    ;

fragment REGULAR_EXPRESSION_OPTION
    :   [imxo]
    ;

fragment REGULAR_EXPRESSION_CHARACTER
    :   ~[/#\\]
    |   '#' {_input.LA(1) != '$' && _input.LA(1) != '@' && _input.LA(1) != '{'}?
    |   REGULAR_EXPRESSION_NON_ESCAPED_SEQUENCE
    |   REGULAR_EXPRESSION_ESCAPE_SEQUENCE
    |   LINE_TERMINATOR_ESCAPE_SEQUENCE
    |   INTERPOLATED_CHARACTER_SEQUENCE
    ;

fragment REGULAR_EXPRESSION_NON_ESCAPED_SEQUENCE
    :   '\\' REGULAR_EXPRESSION_NON_ESCAPED_CHARACTER
    ;

fragment REGULAR_EXPRESSION_NON_ESCAPED_CHARACTER
    :   ~[\r\n]
    |   '\n' {_input.LA(1) != '\r'}?
    ;

fragment REGULAR_EXPRESSION_ESCAPE_SEQUENCE
    :   '\\' '/'
    ;

// --------------------------------------------------------
// Data section mode
// --------------------------------------------------------

mode DATA_SECTION_MODE;

DATA_SECTION_CONTENT
    :   .*? EOF
        -> popMode, skip
    ;
