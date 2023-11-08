parser grammar DeprecatedRubyParser;

@header {
    package io.joern.rubysrc2cpg.deprecated.parser;
}

options {
    tokenVocab = DeprecatedRubyLexer;
}

// --------------------------------------------------------
// Program
// --------------------------------------------------------

program
    :   compoundStatement EOF
    ;

compoundStatement
    :   (SEMI | NL)* statements? (SEMI | NL)*
    ;

// --------------------------------------------------------
// Statements
// --------------------------------------------------------

statements
    :   statement ((SEMI | NL)+ statement)*
    ;

statement
    :   ALIAS NL? definedMethodNameOrSymbol NL? definedMethodNameOrSymbol                                           # aliasStatement
    |   UNDEF NL? definedMethodNameOrSymbol (COMMA NL? definedMethodNameOrSymbol)*                                  # undefStatement
    |   BEGIN_ LCURLY compoundStatement RCURLY                                                                      # beginStatement
    |   END_ LCURLY compoundStatement RCURLY                                                                        # endStatement
    |   statement mod=(IF | UNLESS | WHILE | UNTIL | RESCUE) NL? statement                                          # modifierStatement
    |   expressionOrCommand                                                                                         # expressionOrCommandStatement
    ;

// --------------------------------------------------------
// Expressions
// --------------------------------------------------------

expressionOrCommand
    :   expression                                                                                                  # expressionExpressionOrCommand
    |   (EMARK NL?)? invocationWithoutParentheses                                                                   # invocationExpressionOrCommand
    |   NOT NL? expressionOrCommand                                                                                 # notExpressionOrCommand
    |   <assoc=right> expressionOrCommand op=(OR | AND) NL? expressionOrCommand                                     # orAndExpressionOrCommand
    ;

expression
    :   <assoc=right> singleLeftHandSide op=(EQ | ASSIGNMENT_OPERATOR) NL? multipleRightHandSide                    # singleAssignmentExpression
    |   <assoc=right> multipleLeftHandSide EQ NL? multipleRightHandSide                                             # multipleAssignmentExpression
    |   primary                                                                                                     # primaryExpression
    |   op=(TILDE | PLUS | EMARK) NL? expression                                                                    # unaryExpression
    |   <assoc=right> expression STAR2 NL? expression                                                               # powerExpression
    |   MINUS NL? expression                                                                                        # unaryMinusExpression
    |   expression op=(STAR | SLASH | PERCENT) NL? expression                                                       # multiplicativeExpression
    |   expression op=(PLUS | MINUS) NL? expression                                                                 # additiveExpression
    |   expression op=(LT2 | GT2) NL? expression                                                                    # bitwiseShiftExpression
    |   expression op=AMP NL? expression                                                                            # bitwiseAndExpression
    |   expression op=(BAR | CARET) NL? expression                                                                  # bitwiseOrExpression
    |   expression op=(GT | GTEQ | LT | LTEQ) NL? expression                                                        # relationalExpression
    |   expression op=(LTEQGT | EQ2 | EQ3 | EMARKEQ | EQTILDE | EMARKTILDE) NL? expression?                         # equalityExpression
    |   expression op=AMP2 NL? expression                                                                           # operatorAndExpression
    |   expression op=BAR2 NL? expression                                                                           # operatorOrExpression
    |   expression op=(DOT2 | DOT3) NL? expression?                                                                 # rangeExpression
    |   expression QMARK NL? expression NL? COLON NL? expression                                                    # conditionalOperatorExpression
    |   IS_DEFINED NL? expression                                                                                   # isDefinedExpression
    ;

primary
    :   classDefinition                                                                                                     # classDefinitionPrimary
    |   moduleDefinition                                                                                                    # moduleDefinitionPrimary
    |   methodDefinition                                                                                                    # methodDefinitionPrimary
    |   procDefinition                                                                                                      # procDefinitionPrimary
    |   yieldWithOptionalArgument                                                                                           # yieldWithOptionalArgumentPrimary
    |   ifExpression                                                                                                        # ifExpressionPrimary
    |   unlessExpression                                                                                                    # unlessExpressionPrimary
    |   caseExpression                                                                                                      # caseExpressionPrimary
    |   whileExpression                                                                                                     # whileExpressionPrimary
    |   untilExpression                                                                                                     # untilExpressionPrimary
    |   forExpression                                                                                                       # forExpressionPrimary
    |   RETURN argumentsWithParentheses                                                                                     # returnWithParenthesesPrimary
    |   jumpExpression                                                                                                      # jumpExpressionPrimary
    |   beginExpression                                                                                                     # beginExpressionPrimary
    |   LPAREN compoundStatement RPAREN                                                                                     # groupingExpressionPrimary
    |   variableReference                                                                                                   # variableReferencePrimary
    |   COLON2 CONSTANT_IDENTIFIER                                                                                          # simpleScopedConstantReferencePrimary
    |   primary COLON2 CONSTANT_IDENTIFIER                                                                                  # chainedScopedConstantReferencePrimary
    |   arrayConstructor                                                                                                    # arrayConstructorPrimary
    |   hashConstructor                                                                                                     # hashConstructorPrimary
    |   literal                                                                                                             # literalPrimary
    |   stringExpression                                                                                                    # stringExpressionPrimary
    |   stringInterpolation                                                                                                 # stringInterpolationPrimary
    |   quotedStringExpression                                                                                              # quotedStringExpressionPrimary
    |   regexInterpolation                                                                                                  # regexInterpolationPrimary
    |   quotedRegexInterpolation                                                                                            # quotedRegexInterpolationPrimary
    |   IS_DEFINED LPAREN expressionOrCommand RPAREN                                                                        # isDefinedPrimary
    |   SUPER argumentsWithParentheses? block?                                                                              # superExpressionPrimary
    |   primary LBRACK indexingArguments? RBRACK                                                                            # indexingExpressionPrimary
    |   methodOnlyIdentifier                                                                                                # methodOnlyIdentifierPrimary
    |   methodIdentifier block                                                                                              # invocationWithBlockOnlyPrimary
    |   methodIdentifier argumentsWithParentheses block?                                                                    # invocationWithParenthesesPrimary
    |   primary NL? (DOT | COLON2| AMPDOT) NL? methodName argumentsWithParentheses? block?                                  # chainedInvocationPrimary
    |   primary COLON2 methodName block?                                                                                    # chainedInvocationWithoutArgumentsPrimary
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
    :   (multipleLeftHandSideItem COMMA NL?)+ (multipleLeftHandSideItem | packingLeftHandSide)?                     # multipleLeftHandSideAndpackingLeftHandSideMultipleLeftHandSide
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
    :   expressionOrCommands (COMMA NL? splattingArgument)?
    |   splattingArgument
    ;

expressionOrCommands
    :   expressionOrCommand (COMMA NL? expressionOrCommand)*
    ;

// --------------------------------------------------------
// Invocation expressions
// --------------------------------------------------------

invocationWithoutParentheses
    :   chainedCommandWithDoBlock                                                                                               # chainedCommandDoBlockInvocationWithoutParentheses
    |   command                                                                                                                 # singleCommandOnlyInvocationWithoutParentheses
    |   RETURN arguments?                                                                                                       # returnArgsInvocationWithoutParentheses
    |   BREAK arguments                                                                                                         # breakArgsInvocationWithoutParentheses
    |   NEXT arguments                                                                                                          # nextArgsInvocationWithoutParentheses
    ;

command
    :   SUPER argumentsWithoutParentheses                                                                                               # superCommand
    |   YIELD argumentsWithoutParentheses                                                                                               # yieldCommand
    |   methodIdentifier argumentsWithoutParentheses                                                                                    # simpleMethodCommand
    |   primary (DOT | COLON2| AMPDOT) NL? methodName argumentsWithoutParentheses                                                       # memberAccessCommand
    ;

chainedCommandWithDoBlock
    :   commandWithDoBlock ((DOT | COLON2) methodName argumentsWithParentheses?)*
    ;

commandWithDoBlock
    :   SUPER argumentsWithoutParentheses doBlock                                                                               # argsAndDoBlockCommandWithDoBlock
    |   methodIdentifier argumentsWithoutParentheses doBlock                                                                    # argsAndDoBlockAndMethodIdCommandWithDoBlock
    |   primary (DOT | COLON2) methodName argumentsWithoutParentheses doBlock                                                   # primaryMethodArgsDoBlockCommandWithDoBlock
    ;

argumentsWithoutParentheses
    :   arguments
    ;

arguments
    :   argument (COMMA NL? argument)*
    ;
    
argument
    :   HERE_DOC_IDENTIFIER                                                                                                     # hereDocArgument
    |   blockArgument                                                                                                           # blockArgumentArgument
    |   splattingArgument                                                                                                       # splattingArgumentArgument
    |   association                                                                                                             # associationArgument
    |   expression                                                                                                              # expressionArgument
    |   command                                                                                                                 # commandArgument
    ;

blockArgument
    :   AMP expression
    ;

// --------------------------------------------------------
// Arguments
// --------------------------------------------------------

splattingArgument
    :   STAR expressionOrCommand
    |   STAR2 expressionOrCommand
    ;

indexingArguments
    :   expressions (COMMA NL?)?                                                                                                # expressionsOnlyIndexingArguments
    |   expressions COMMA NL? splattingArgument                                                                                 # expressionsAndSplattingIndexingArguments
    |   associations (COMMA NL?)?                                                                                               # associationsOnlyIndexingArguments
    |   splattingArgument                                                                                                       # splattingOnlyIndexingArguments
    |   command                                                                                                                 # commandOnlyIndexingArguments
    ;

argumentsWithParentheses
    :   LPAREN NL? RPAREN                                                                                                       # blankArgsArgumentsWithParentheses
    |   LPAREN NL? arguments (COMMA)? NL? RPAREN                                                                                # argsOnlyArgumentsWithParentheses
    |   LPAREN NL? expressions COMMA NL? chainedCommandWithDoBlock NL? RPAREN                                                   # expressionsAndChainedCommandWithDoBlockArgumentsWithParentheses
    |   LPAREN NL? chainedCommandWithDoBlock NL? RPAREN                                                                         # chainedCommandWithDoBlockOnlyArgumentsWithParentheses
    ;

expressions
    :   expression (COMMA NL? expression)*
    ;

// --------------------------------------------------------
// Blocks
// --------------------------------------------------------

block
    :   braceBlock                                                                                                              # braceBlockBlock
    |   doBlock                                                                                                                 # doBlockBlock
    ;

braceBlock
    :   LCURLY NL? blockParameter? bodyStatement RCURLY
    ;

doBlock
    :   DO NL? blockParameter? bodyStatement END
    ;

blockParameter
    :   BAR blockParameters? BAR
    ;

blockParameters
    :   singleLeftHandSide
    |   multipleLeftHandSide
    ;

// --------------------------------------------------------
// Arrays
// --------------------------------------------------------

arrayConstructor
    :   LBRACK NL? indexingArguments? NL? RBRACK                                                                    # bracketedArrayConstructor
    |   QUOTED_NON_EXPANDED_STRING_ARRAY_LITERAL_START
        nonExpandedArrayElements?
        QUOTED_NON_EXPANDED_STRING_ARRAY_LITERAL_END                                                                # nonExpandedWordArrayConstructor
    |   QUOTED_NON_EXPANDED_SYMBOL_ARRAY_LITERAL_START
        nonExpandedArrayElements?
        QUOTED_NON_EXPANDED_SYMBOL_ARRAY_LITERAL_END                                                                # nonExpandedSymbolArrayConstructor
    |   QUOTED_EXPANDED_SYMBOL_ARRAY_LITERAL_START
        expandedArrayElements?
        QUOTED_EXPANDED_SYMBOL_ARRAY_LITERAL_END                                                                    # expandedSymbolArrayConstructor
    |   QUOTED_EXPANDED_STRING_ARRAY_LITERAL_START
        expandedArrayElements?
        QUOTED_EXPANDED_STRING_ARRAY_LITERAL_END                                                                    # expandedWordArrayConstructor
    ;


expandedArrayElements
    :   EXPANDED_ARRAY_ITEM_SEPARATOR*
        expandedArrayElement (EXPANDED_ARRAY_ITEM_SEPARATOR+ expandedArrayElement)*
        EXPANDED_ARRAY_ITEM_SEPARATOR*
    ;
    
expandedArrayElement
    :   (EXPANDED_ARRAY_ITEM_CHARACTER | delimitedArrayItemInterpolation)+
    ;

delimitedArrayItemInterpolation
    :   DELIMITED_ARRAY_ITEM_INTERPOLATION_BEGIN
        compoundStatement
        DELIMITED_ARRAY_ITEM_INTERPOLATION_END
    ;

nonExpandedArrayElements
    :   NON_EXPANDED_ARRAY_ITEM_SEPARATOR*
        nonExpandedArrayElement (NON_EXPANDED_ARRAY_ITEM_SEPARATOR+ nonExpandedArrayElement)*
        NON_EXPANDED_ARRAY_ITEM_SEPARATOR*
    ;

nonExpandedArrayElement
    :   NON_EXPANDED_ARRAY_ITEM_CHARACTER+
    ;

// --------------------------------------------------------
// Hashes
// --------------------------------------------------------

hashConstructor
    :   LCURLY NL? (hashConstructorElements COMMA?)? NL? RCURLY
    ;

hashConstructorElements
    :   hashConstructorElement (COMMA NL? hashConstructorElement)*
    ;

hashConstructorElement
    :   association
    |   STAR2 expression
    ;

associations
    :   association (COMMA NL? association)*
    ;

association
    :   (expression | keyword) (EQGT|COLON) (NL? expression)?
    ;

// --------------------------------------------------------
// Method definitions
// --------------------------------------------------------

methodDefinition
    :   DEF NL? methodNamePart methodParameterPart bodyStatement END
    |   DEF NL? methodIdentifier methodParameterPart EQ NL? expression
    ;
    

procDefinition
    :   MINUSGT (LPAREN parameters? RPAREN)? block
    ;

methodNamePart
    :   definedMethodName                                                                                           # simpleMethodNamePart
    |   singletonObject NL? (DOT | COLON2) NL? definedMethodName                                                    # singletonMethodNamePart
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
    :   ASSIGNMENT_LIKE_METHOD_IDENTIFIER
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
    :   LPAREN NL? parameters? NL? RPAREN
    |   parameters?
    ;

parameters
    :   parameter (COMMA NL? parameter)*
    ;
    
parameter
    :   optionalParameter   
    |   mandatoryParameter
    |   arrayParameter
    |   hashParameter
    |   keywordParameter
    |   procParameter
    ;

mandatoryParameter
    :   LOCAL_VARIABLE_IDENTIFIER
    ;

optionalParameter
    :   LOCAL_VARIABLE_IDENTIFIER EQ NL? expression
    ;

arrayParameter
    :   STAR LOCAL_VARIABLE_IDENTIFIER?
    ;

hashParameter
    :   STAR2 LOCAL_VARIABLE_IDENTIFIER?
    ;

keywordParameter
    :   LOCAL_VARIABLE_IDENTIFIER COLON (NL? expression)?
    ;

procParameter
    :   AMP LOCAL_VARIABLE_IDENTIFIER?
    ;


// --------------------------------------------------------
// Conditional expressions
// --------------------------------------------------------

ifExpression
    :   IF NL? expressionOrCommand thenClause elsifClause* elseClause? END
    ;

thenClause
    :   (SEMI | NL)+ compoundStatement
    |   (SEMI | NL)? THEN compoundStatement
    ;

elsifClause
    :   ELSIF NL? expressionOrCommand thenClause
    ;

elseClause
    :   ELSE compoundStatement
    ;

unlessExpression
    :   UNLESS NL? expressionOrCommand thenClause elseClause? END
    ;

caseExpression
    :   CASE NL? expressionOrCommand? (SEMI | NL)* whenClause+ elseClause? END
    ;

whenClause
    :   WHEN NL? whenArgument thenClause
    ;

whenArgument
    :   expressions (COMMA splattingArgument)?
    |   splattingArgument
    ;

// --------------------------------------------------------
// Iteration expressions
// --------------------------------------------------------

whileExpression
    :   WHILE NL? expressionOrCommand doClause END
    ;

doClause
    :   (SEMI | NL)+ compoundStatement
    |   DO compoundStatement
    ;

untilExpression
    :   UNTIL NL? expressionOrCommand doClause END
    ;

forExpression
    :   FOR NL? forVariable IN NL? expressionOrCommand doClause END
    ;

forVariable
    :   singleLeftHandSide
    |   multipleLeftHandSide
    ;

// --------------------------------------------------------
// Begin expression
// --------------------------------------------------------

beginExpression
    :   BEGIN bodyStatement END
    ;

bodyStatement
    :   compoundStatement rescueClause* elseClause? ensureClause?
    ;

rescueClause
    :   RESCUE exceptionClass? NL? exceptionVariableAssignment? thenClause
    ;

exceptionClass
    :   expression
    |   multipleRightHandSide
    ;

exceptionVariableAssignment
    :   EQGT singleLeftHandSide
    ;

ensureClause
    :   ENSURE compoundStatement
    ;

// --------------------------------------------------------
// Class definitions
// --------------------------------------------------------

classDefinition
    :   CLASS NL? classOrModuleReference (LT NL? expressionOrCommand)? bodyStatement END
    |   CLASS NL? LT2 NL? expressionOrCommand (SEMI | NL)+ bodyStatement END
    ;

classOrModuleReference
    :   scopedConstantReference
    |   CONSTANT_IDENTIFIER
    ;

// --------------------------------------------------------
// Module definitions
// --------------------------------------------------------

moduleDefinition
    :   MODULE NL? classOrModuleReference bodyStatement END
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
    :   BREAK
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
    :   HERE_DOC                                                                                                    # hereDocLiteral
    |   numericLiteral                                                                                              # numericLiteralLiteral
    |   symbol                                                                                                      # symbolLiteral
    |   REGULAR_EXPRESSION_START REGULAR_EXPRESSION_BODY? REGULAR_EXPRESSION_END                                    # regularExpressionLiteral
    ;
    
symbol
    :   SYMBOL_LITERAL
    |   COLON stringExpression
    ;

// --------------------------------------------------------
// Strings
// --------------------------------------------------------

stringExpression
    :   simpleString                                                                                                # simpleStringExpression
    |   stringInterpolation                                                                                         # interpolatedStringExpression
    |   stringExpression stringExpression+                                                                          # concatenatedStringExpression
    ;

quotedStringExpression
    :   QUOTED_NON_EXPANDED_STRING_LITERAL_START 
        NON_EXPANDED_LITERAL_CHARACTER_SEQUENCE? 
        QUOTED_NON_EXPANDED_STRING_LITERAL_END                                                                      # nonExpandedQuotedStringLiteral
    |   QUOTED_EXPANDED_STRING_LITERAL_START
        (EXPANDED_LITERAL_CHARACTER_SEQUENCE | delimitedStringInterpolation)*
        QUOTED_EXPANDED_STRING_LITERAL_END                                                                          # expandedQuotedStringLiteral
    |   QUOTED_EXPANDED_EXTERNAL_COMMAND_LITERAL_START
        (EXPANDED_LITERAL_CHARACTER_SEQUENCE | delimitedStringInterpolation)*
        QUOTED_EXPANDED_EXTERNAL_COMMAND_LITERAL_END                                                                # expandedExternalCommandLiteral
    ;

simpleString
    :   SINGLE_QUOTED_STRING_LITERAL                                                                                # singleQuotedStringLiteral
    |   DOUBLE_QUOTED_STRING_START DOUBLE_QUOTED_STRING_CHARACTER_SEQUENCE? DOUBLE_QUOTED_STRING_END                # doubleQuotedStringLiteral
    ;

delimitedStringInterpolation
    :   DELIMITED_STRING_INTERPOLATION_BEGIN
        compoundStatement
        DELIMITED_STRING_INTERPOLATION_END
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

quotedRegexInterpolation
    :   QUOTED_EXPANDED_REGULAR_EXPRESSION_START
        (EXPANDED_LITERAL_CHARACTER_SEQUENCE | delimitedStringInterpolation)*
        QUOTED_EXPANDED_REGULAR_EXPRESSION_END
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
