parser grammar RubyParser;

options {
    tokenVocab = RubyLexer;
}

// --------------------------------------------------------
// Program
// --------------------------------------------------------

program
    :   wsOrNl* compoundStatement EOF
    ;

compoundStatement
    :   statements? separators?
    ;

separators
    :   WS* separator (WS* separator)*
    ;

separator
    :   SEMI
    |   NL
    ;

// --------------------------------------------------------
// Statements
// --------------------------------------------------------

statements
    :   statement (separators WS* statement)*
    ;

statement
    :   ALIAS wsOrNl* definedMethodNameOrSymbol wsOrNl* definedMethodNameOrSymbol                                   # aliasStatement
    |   UNDEF wsOrNl* definedMethodNameOrSymbol (wsOrNl* COMMA wsOrNl* definedMethodNameOrSymbol)*                  # undefStatement
    |   statement WS* mod=(IF | UNLESS | WHILE | UNTIL | RESCUE) wsOrNl* statement                                  # modifierStatement
    |   BEGIN_ wsOrNl* LCURLY wsOrNl* statements? wsOrNl* RCURLY                                                    # beginStatement
    |   END_ wsOrNl* LCURLY wsOrNl* statements? wsOrNl* RCURLY                                                      # endStatement
    |   expressionOrCommand                                                                                         # expressionOrCommandStatement
    ;

// --------------------------------------------------------
// Expressions
// --------------------------------------------------------

expressionOrCommand
    :   expression                                                                                                  # expressionExpressionOrCommand
    |   (EMARK wsOrNl*)? invocationWithoutParentheses                                                               # invocationExpressionOrCommand
    |   NOT wsOrNl* expressionOrCommand                                                                             # notExpressionOrCommand
    |   <assoc=right> expressionOrCommand WS* op=(OR | AND) wsOrNl* expressionOrCommand                             # orAndExpressionOrCommand
    ;

expression
    :   primary                                                                                                     # primaryExpression
    |   op=(TILDE | PLUS | EMARK) wsOrNl* expression                                                                # unaryExpression
    |   <assoc=right> expression WS* STAR2 wsOrNl* expression                                                       # powerExpression
    |   MINUS wsOrNl* expression                                                                                    # unaryMinusExpression
    |   expression WS* op=(STAR | SLASH | PERCENT) wsOrNl* expression                                               # multiplicativeExpression
    |   expression WS* op=(PLUS | MINUS) wsOrNl* expression                                                         # additiveExpression
    |   expression WS* op=(LT2 | GT2) wsOrNl* expression                                                            # bitwiseShiftExpression
    |   expression WS* op=AMP wsOrNl* expression                                                                    # bitwiseAndExpression
    |   expression WS* op=(BAR | CARET) wsOrNl* expression                                                          # bitwiseOrExpression
    |   expression WS* op=(GT | GTEQ | LT | LTEQ) wsOrNl* expression                                                # relationalExpression
    |   expression WS* op=(LTEQGT | EQ2 | EQ3 | EMARKEQ | EQTILDE | EMARKTILDE) wsOrNl* expression?                 # equalityExpression
    |   expression WS* op=AMP2 wsOrNl* expression                                                                   # operatorAndExpression
    |   expression WS* op=BAR2 wsOrNl* expression                                                                   # operatorOrExpression
    |   expression WS* op=(DOT2 | DOT3) wsOrNl* expression?                                                         # rangeExpression
    |   expression WS* QMARK wsOrNl* expression WS* COLON wsOrNl* expression                                        # conditionalOperatorExpression
    |   <assoc=right> singleLeftHandSide WS* op=(EQ | ASSIGNMENT_OPERATOR) wsOrNl* multipleRightHandSide            # singleAssignmentExpression
    |   <assoc=right> multipleLeftHandSide WS* EQ wsOrNl* multipleRightHandSide                                     # multipleAssignmentExpression
    |   IS_DEFINED wsOrNl* expression                                                                               # isDefinedExpression
    ;

primary
    :   classDefinition                                                                                             # classDefinitionPrimary
    |   moduleDefinition                                                                                            # moduleDefinitionPrimary
    |   methodDefinition                                                                                            # methodDefinitionPrimary
    |   procDefinition                                                                                              # procDefinitionPrimary
    |   yieldWithOptionalArgument                                                                                   # yieldWithOptionalArgumentPrimary
    |   ifExpression                                                                                                # ifExpressionPrimary
    |   unlessExpression                                                                                            # unlessExpressionPrimary
    |   caseExpression                                                                                              # caseExpressionPrimary
    |   whileExpression                                                                                             # whileExpressionPrimary
    |   untilExpression                                                                                             # untilExpressionPrimary
    |   forExpression                                                                                               # forExpressionPrimary
    |   jumpExpression                                                                                              # jumpExpressionPrimary
    |   beginExpression                                                                                             # beginExpressionPrimary
    |   LPAREN wsOrNl* compoundStatement wsOrNl* RPAREN                                                             # groupingExpressionPrimary
    |   variableReference                                                                                           # variableReferencePrimary
    |   COLON2 CONSTANT_IDENTIFIER                                                                                  # simpleScopedConstantReferencePrimary
    |   primary COLON2 CONSTANT_IDENTIFIER                                                                          # chainedScopedConstantReferencePrimary
    |   arrayConstructor                                                                                            # arrayConstructorPrimary
    |   hashConstructor                                                                                             # hashConstructorPrimary
    |   literal                                                                                                     # literalPrimary
    |   stringExpression                                                                                            # stringExpressionPrimary
    |   stringInterpolation                                                                                         # stringInterpolationPrimary
    |   regexInterpolation                                                                                          # regexInterpolationPrimary
    |   IS_DEFINED LPAREN expressionOrCommand RPAREN                                                                # isDefinedPrimary
    |   SUPER argumentsWithParentheses? block?                                                                      # superExpressionPrimary
    |   primary LBRACK WS* indexingArguments? WS* RBRACK                                                            # indexingExpressionPrimary
    |   methodOnlyIdentifier                                                                                        # methodOnlyIdentifierPrimary
    |   methodIdentifier WS? block                                                                                  # invocationWithBlockOnlyPrimary
    |   methodIdentifier argumentsWithParentheses WS* block?                                                        # invocationWithParenthesesPrimary
    |   primary (DOT | COLON2) wsOrNl* methodName argumentsWithParentheses? WS? block?                              # chainedInvocationPrimary
    |   primary COLON2 methodName block?                                                                            # chainedInvocationWithoutArgumentsPrimary
    ;

// --------------------------------------------------------
// Assignments
// --------------------------------------------------------

singleLeftHandSide
    :   variableIdentifier                                                                                          # variableIdentifierOnlySingleLeftHandSide
    |   primary LBRACK arguments? RBRACK                                                                            # primaryInsideBracketsSingleLeftHandSide
    |   primary (DOT | COLON2) (LOCAL_VARIABLE_IDENTIFIER | CONSTANT_IDENTIFIER)                                    # xdotySingleLeftHandSide
    |   COLON2 CONSTANT_IDENTIFIER                                                                                  # scopedConstantAccessSingleLeftHandSide
    ;

multipleLeftHandSide
    :   (multipleLeftHandSideItem COMMA wsOrNl*)+ (multipleLeftHandSideItem | packingLeftHandSide)?                 # multipleLeftHandSideAndpackingLeftHandSideMultipleLeftHandSide
    |   packingLeftHandSide                                                                                         # packingLeftHandSideOnlyMultipleLeftHandSide
    |   groupedLeftHandSide                                                                                         # groupedLeftHandSideOnlyMultipleLeftHandSide
    ;

multipleLeftHandSideItem
    :   singleLeftHandSide
    |   groupedLeftHandSide
    ;

packingLeftHandSide
    :   STAR singleLeftHandSide
    ;

groupedLeftHandSide
    :   LPAREN multipleLeftHandSide RPAREN
    ;

multipleRightHandSide
    :   expressionOrCommands (WS* COMMA wsOrNl* splattingArgument)?
    |   splattingArgument
    ;

expressionOrCommands
    :   expressionOrCommand (WS* COMMA wsOrNl* expressionOrCommand)*
    ;

// --------------------------------------------------------
// Invocation expressions
// --------------------------------------------------------

invocationWithoutParentheses
    :   chainedCommandWithDoBlock                                                                                               # chainedCommandDoBlockInvocationWithoutParentheses
    |   command                                                                                                                 # singleCommandOnlyInvocationWithoutParentheses
    |   RETURN WS arguments                                                                                                     # returnArgsInvocationWithoutParentheses
    |   BREAK WS arguments                                                                                                      # breakArgsInvocationWithoutParentheses
    |   NEXT WS arguments                                                                                                       # nextArgsInvocationWithoutParentheses
    ;

command
    :   SUPER argumentsWithoutParentheses                                                                                       # superCommand
    |   YIELD argumentsWithoutParentheses                                                                                       # yieldCommand
    |   methodIdentifier argumentsWithoutParentheses                                                                            # simpleMethodCommand
    |   primary WS* (DOT | COLON2) wsOrNl* methodName argumentsWithoutParentheses                                               # memberAccessCommand
    ;

chainedCommandWithDoBlock
    :   commandWithDoBlock ((DOT | COLON2) methodName argumentsWithParentheses?)*
    ;

commandWithDoBlock
    :   SUPER argumentsWithoutParentheses WS* doBlock                                                                           # argsAndDoBlockCommandWithDoBlock
    |   methodIdentifier argumentsWithoutParentheses WS* doBlock                                                                # argsAndDoBlockAndMethodIdCommandWithDoBlock
    |   primary WS* (DOT | COLON2) methodName argumentsWithoutParentheses WS* doBlock                                           # primaryMethodArgsDoBlockCommandWithDoBlock
    ;

argumentsWithoutParentheses
    :   WS+ arguments
    ;

arguments
    :   argument (WS* COMMA wsOrNl* argument)*
    ;
    
argument
    :   blockArgument                                                                                                           # blockArgumentArgument
    |   splattingArgument                                                                                                       # splattingArgumentArgument
    |   expression                                                                                                              # expressionArgument
    |   association                                                                                                             # associationArgument
    |   command                                                                                                                 # commandArgument
    ;

blockArgument
    :   AMP expression
    ;

// --------------------------------------------------------
// Arguments
// --------------------------------------------------------

splattingArgument
    :   STAR WS* expressionOrCommand
    |   STAR2 WS* expressionOrCommand
    ;

indexingArguments
    :   command                                                                                                                 # commandOnlyIndexingArguments
    |   expressions (WS* COMMA wsOrNl*)?                                                                                        # expressionsOnlyIndexingArguments
    |   expressions WS* COMMA wsOrNl* splattingArgument                                                                         # expressionsAndSplattingIndexingArguments
    |   associations (WS* COMMA wsOrNl*)?                                                                                       # associationsOnlyIndexingArguments
    |   splattingArgument                                                                                                       # splattingOnlyIndexingArguments
    ;

argumentsWithParentheses
    :   LPAREN wsOrNl* RPAREN                                                                                                   # blankArgsArgumentsWithParentheses
    |   LPAREN wsOrNl* arguments (WS* COMMA)? wsOrNl* RPAREN                                                                    # argsOnlyArgumentsWithParentheses
    |   LPAREN wsOrNl* expressions WS* COMMA wsOrNl* chainedCommandWithDoBlock wsOrNl* RPAREN                                   # expressionsAndChainedCommandWithDoBlockArgumentsWithParentheses
    |   LPAREN wsOrNl* chainedCommandWithDoBlock wsOrNl* RPAREN                                                                 # chainedCommandWithDoBlockOnlyArgumentsWithParentheses
    ;

expressions
    :   expression (WS* COMMA wsOrNl* expression)*
    ;

// --------------------------------------------------------
// Blocks
// --------------------------------------------------------

block
    :   braceBlock                                                                                                              # braceBlockBlock
    |   doBlock                                                                                                                 # doBlockBlock
    ;

braceBlock
    :   LCURLY wsOrNl* blockParameter? wsOrNl* compoundStatement wsOrNl* RCURLY
    ;

doBlock
    :   DO wsOrNl* blockParameter? separators wsOrNl* compoundStatement wsOrNl* END
    ;

blockParameter
    :   BAR WS* blockParameters? WS* BAR
    ;

blockParameters
    :   singleLeftHandSide
    |   multipleLeftHandSide
    ;

// --------------------------------------------------------
// Arrays
// --------------------------------------------------------

arrayConstructor
    :   LBRACK wsOrNl* indexingArguments? wsOrNl* RBRACK
    ;

// --------------------------------------------------------
// Hashes
// --------------------------------------------------------

hashConstructor
    :   LCURLY wsOrNl* (associations WS* COMMA?)? wsOrNl* RCURLY
    ;

associations
    :   association (WS* COMMA wsOrNl* association)*
    ;

association
    :   (expression | keyword) WS* (EQGT|COLON) wsOrNl* expression
    ;

// --------------------------------------------------------
// Method definitions
// --------------------------------------------------------

methodDefinition
    :   DEF wsOrNl* methodNamePart WS* methodParameterPart wsOrNl* bodyStatement wsOrNl* END
    ;

procDefinition
    :   MINUSGT WS? (LPAREN parameters? RPAREN)? WS? block
    ;

methodNamePart
    :   definedMethodName                                                                                           # simpleMethodNamePart
    |   singletonObject wsOrNl* (DOT | COLON2) wsOrNl* definedMethodName                                            # singletonMethodNamePart
    ;

singletonObject
    :   variableIdentifier
    |   pseudoVariableIdentifier
    |   LPAREN expressionOrCommand RPAREN
    ;

definedMethodName
    :   methodName
    |   assignmentLikeMethodIdentifier
    ;

assignmentLikeMethodIdentifier
    :   (CONSTANT_IDENTIFIER | LOCAL_VARIABLE_IDENTIFIER) EQ
    ;

methodName
    :   methodIdentifier
    |   operatorMethodName
    |   keyword
    ;

methodIdentifier
    :   LOCAL_VARIABLE_IDENTIFIER
    |   CONSTANT_IDENTIFIER
    |   methodOnlyIdentifier
    ;

methodOnlyIdentifier
    :   (LOCAL_VARIABLE_IDENTIFIER | CONSTANT_IDENTIFIER | keyword) (EMARK | QMARK)
    ;

methodParameterPart
    :   LPAREN parameters? RPAREN
    |   parameters? separator
    ;

parameters
    :   parameter (WS* COMMA wsOrNl* parameter)*
    ;
    
parameter
    :   mandatoryParameter
    |   optionalParameter
    |   arrayParameter
    |   hashParameter
    |   keywordParameter
    |   procParameter
    ;

mandatoryParameter
    :   LOCAL_VARIABLE_IDENTIFIER
    ;

optionalParameter
    :   LOCAL_VARIABLE_IDENTIFIER WS* EQ wsOrNl* expression
    ;

arrayParameter
    :   STAR LOCAL_VARIABLE_IDENTIFIER?
    ;

hashParameter
    :   STAR2 LOCAL_VARIABLE_IDENTIFIER?
    ;

keywordParameter
    :   LOCAL_VARIABLE_IDENTIFIER WS* COLON wsOrNl* expression
    ;

procParameter
    :   AMP LOCAL_VARIABLE_IDENTIFIER
    ;

// --------------------------------------------------------
// Conditional expressions
// --------------------------------------------------------

ifExpression
    :   IF wsOrNl* expressionOrCommand WS* thenClause (wsOrNl* elsifClause)* (wsOrNl* elseClause)? wsOrNl* END
    ;

thenClause
    :   separator wsOrNl* compoundStatement
    |   separator? THEN wsOrNl* compoundStatement
    ;

elsifClause
    :   ELSIF wsOrNl* expressionOrCommand WS? thenClause
    ;

elseClause
    :   ELSE wsOrNl* compoundStatement
    ;

unlessExpression
    :   UNLESS wsOrNl* expressionOrCommand WS* thenClause wsOrNl* elseClause? wsOrNl* END
    ;

caseExpression
    :   CASE (wsOrNl* expressionOrCommand)? separators? (WS* whenClause WS*)+ elseClause? wsOrNl* END
    ;

whenClause
    :   WHEN wsOrNl* whenArgument WS* thenClause
    ;

whenArgument
    :   expressions (WS* COMMA splattingArgument)?
    |   splattingArgument
    ;

// --------------------------------------------------------
// Iteration expressions
// --------------------------------------------------------

whileExpression
    :   WHILE wsOrNl* expressionOrCommand doClause wsOrNl* END
    ;

doClause
    :   separator wsOrNl* compoundStatement
    |   WS? DO wsOrNl* compoundStatement
    ;

untilExpression
    :   UNTIL wsOrNl* expressionOrCommand doClause wsOrNl* END
    ;

forExpression
    :   FOR wsOrNl* forVariable WS* IN wsOrNl* expressionOrCommand doClause wsOrNl* END
    ;

forVariable
    :   singleLeftHandSide
    |   multipleLeftHandSide
    ;

// --------------------------------------------------------
// Begin expression
// --------------------------------------------------------

beginExpression
    :   BEGIN wsOrNl* bodyStatement wsOrNl* END
    ;

bodyStatement
    :   compoundStatement (wsOrNl* rescueClause)* (wsOrNl* elseClause)? ensureClause?
    ;

rescueClause
    :   RESCUE WS* exceptionClass? wsOrNl* exceptionVariableAssignment? wsOrNl* thenClause
    ;

exceptionClass
    :   expression
    |   multipleRightHandSide
    ;

exceptionVariableAssignment
    :   EQGT WS* singleLeftHandSide
    ;

ensureClause
    :   ENSURE wsOrNl* compoundStatement
    ;

// --------------------------------------------------------
// Class definitions
// --------------------------------------------------------

classDefinition
    :   CLASS wsOrNl* classOrModuleReference WS* (LT wsOrNl* expressionOrCommand)? separators wsOrNl* bodyStatement wsOrNl* END
    |   CLASS wsOrNl* LT2 wsOrNl* expressionOrCommand separators wsOrNl* bodyStatement wsOrNl* END
    ;

classOrModuleReference
    :   scopedConstantReference
    |   CONSTANT_IDENTIFIER
    ;

// --------------------------------------------------------
// Module definitions
// --------------------------------------------------------

moduleDefinition
    :   MODULE wsOrNl* classOrModuleReference wsOrNl* bodyStatement wsOrNl* END
    ;

// --------------------------------------------------------
// Yield expressions
// --------------------------------------------------------

yieldWithOptionalArgument
    :   YIELD (LPAREN arguments? RPAREN)?
    ;

// --------------------------------------------------------
// Jump expressions
// --------------------------------------------------------

jumpExpression
    :   RETURN
    |   BREAK
    |   NEXT
    |   REDO
    |   RETRY
    ;

// --------------------------------------------------------
// Variable references
// --------------------------------------------------------

variableReference
    :   variableIdentifier                                                                                          # variableIdentifierVariableReference
    |   pseudoVariableIdentifier                                                                                    # pseudoVariableIdentifierVariableReference
    ;

variableIdentifier
    :   LOCAL_VARIABLE_IDENTIFIER
    |   GLOBAL_VARIABLE_IDENTIFIER
    |   INSTANCE_VARIABLE_IDENTIFIER
    |   CLASS_VARIABLE_IDENTIFIER
    |   CONSTANT_IDENTIFIER
    ;

pseudoVariableIdentifier
    :   NIL                                                                                                         # nilPseudoVariableIdentifier
    |   TRUE                                                                                                        # truePseudoVariableIdentifier
    |   FALSE                                                                                                       # falsePseudoVariableIdentifier
    |   SELF                                                                                                        # selfPseudoVariableIdentifier
    |   FILE__                                                                                                      # filePseudoVariableIdentifier
    |   LINE__                                                                                                      # linePseudoVariableIdentifier
    |   ENCODING__                                                                                                  # encodingPseudoVariableIdentifier
    ;

scopedConstantReference
    :   COLON2 CONSTANT_IDENTIFIER
    |   primary COLON2 CONSTANT_IDENTIFIER
    ;

// --------------------------------------------------------
// Literals
// --------------------------------------------------------

literal
    :   numericLiteral                                                                                              # numericLiteralLiteral
    |   symbol                                                                                                      # symbolLiteral
    |   REGULAR_EXPRESSION_START REGULAR_EXPRESSION_BODY? REGULAR_EXPRESSION_END                                    # regularExpressionLiteral
    ;
    
symbol
    :   SYMBOL_LITERAL
    |   COLON SINGLE_QUOTED_STRING_LITERAL
    ;

// --------------------------------------------------------
// Strings
// --------------------------------------------------------

stringExpression
    :   simpleString                                                                                                # simpleStringExpression
    |   stringInterpolation                                                                                         # interpolatedStringExpression
    |   stringExpression (WS stringExpression)+                                                                     # concatenatedStringExpression
    ;

simpleString
    :   SINGLE_QUOTED_STRING_LITERAL                                                                                # singleQuotedStringLiteral
    |   DOUBLE_QUOTED_STRING_START DOUBLE_QUOTED_STRING_CHARACTER_SEQUENCE? DOUBLE_QUOTED_STRING_END                # doubleQuotedStringLiteral
    ;

stringInterpolation
    :   DOUBLE_QUOTED_STRING_START
        (DOUBLE_QUOTED_STRING_CHARACTER_SEQUENCE | interpolatedStringSequence)+
        DOUBLE_QUOTED_STRING_END
    ;

interpolatedStringSequence
    :   STRING_INTERPOLATION_BEGIN compoundStatement STRING_INTERPOLATION_END
    ;

// --------------------------------------------------------
// Regex interpolation
// --------------------------------------------------------

regexInterpolation
    :   REGULAR_EXPRESSION_START
        (REGULAR_EXPRESSION_BODY | interpolatedRegexSequence)+
        REGULAR_EXPRESSION_END
    ;

interpolatedRegexSequence
    :   REGULAR_EXPRESSION_INTERPOLATION_BEGIN compoundStatement REGULAR_EXPRESSION_INTERPOLATION_END
    ;


// --------------------------------------------------------
// Numerics
// --------------------------------------------------------

numericLiteral
    :   (PLUS | MINUS)? unsignedNumericLiteral
    ;

unsignedNumericLiteral
    :   DECIMAL_INTEGER_LITERAL
    |   BINARY_INTEGER_LITERAL
    |   OCTAL_INTEGER_LITERAL
    |   HEXADECIMAL_INTEGER_LITERAL
    |   FLOAT_LITERAL_WITHOUT_EXPONENT
    |   FLOAT_LITERAL_WITH_EXPONENT
    ;

// --------------------------------------------------------
// Helpers
// --------------------------------------------------------

definedMethodNameOrSymbol
    :   definedMethodName
    |   symbol
    ;

wsOrNl
    :   WS
    |   NL
    ;

keyword
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

operatorMethodName
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
    |   LBRACK RBRACK
    |   LBRACK RBRACK EQ
    ;
