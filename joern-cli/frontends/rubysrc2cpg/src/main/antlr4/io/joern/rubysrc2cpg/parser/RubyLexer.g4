lexer grammar RubyLexer;

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
RCURLY: '}';
COLON: ':';
COLON2: '::';
COMMA: ',';
SEMI: ';';
DOT: '.';
DOT2: '..';
DOT3: '...';
QMARK: '?';
EQGT: '=>';

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
SLASH: '/';
PERCENT: '%';
TILDE: '~';
PLUSAT: '+@';
MINUSAT: '-@';
LBRACKRBRACK: '[]';
LBRACKRBRACKEQ: '[]=';

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
    |   LBRACKRBRACK
    |   LBRACKRBRACKEQ
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
    :   ':' SYMBOL_NAME
    ;

fragment SYMBOL_NAME
    :   INSTANCE_VARIABLE_IDENTIFIER
    |   GLOBAL_VARIABLE_IDENTIFIER
    |   CLASS_VARIABLE_IDENTIFIER
    |   CONSTANT_IDENTIFIER
    |   LOCAL_VARIABLE_IDENTIFIER
    |   METHOD_ONLY_IDENTIFIER
    |   ASSIGNMENT_LIKE_METHOD_IDENTIFIER
    |   OPERATOR_METHOD_NAME
    |   KEYWORD
    ;

// --------------------------------------------------------
// Identifiers
// --------------------------------------------------------

LOCAL_VARIABLE_IDENTIFIER
    :   (LOWERCASE_CHARACTER | '_') IDENTIFIER_CHARACTER*
    ;

GLOBAL_VARIABLE_IDENTIFIER
    :   '$' IDENTIFIER_START_CHARACTER IDENTIFIER_CHARACTER*
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

fragment ASSIGNMENT_LIKE_METHOD_IDENTIFIER
    :   (CONSTANT_IDENTIFIER | LOCAL_VARIABLE_IDENTIFIER) '='
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
// Data section
// --------------------------------------------------------

END_OF_PROGRAM_MARKER
    :   '__END__' {getCharPositionInLine() == 7}? '\r'? '\n'
        -> pushMode(DATA_SECTION_MODE), skip
    ;

mode DATA_SECTION_MODE;

DATA_SECTION_CONTENT
    :   .*? EOF
        -> popMode, skip
    ;
