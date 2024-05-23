parser grammar RubyParser;

@header {
    package io.joern.rubysrc2cpg.parser;
}

options {
    tokenVocab = RubyLexer;
}

// --------------------------------------------------------
// Program
// --------------------------------------------------------

program
    :   compoundStatement EOF
    ;

compoundStatement
    :   statements? (SEMI | NL)*
    ;

statements
    :   (SEMI | NL)*  statement ((SEMI | NL)+ statement)*
    ;

statement
    :   expressionOrCommand
        # expressionOrCommandStatement
    |   ALIAS NL* oldName=definedMethodNameOrSymbol newName=definedMethodNameOrSymbol
        # aliasStatement
    |   UNDEF NL* definedMethodNameOrSymbol (COMMA NL* definedMethodNameOrSymbol)*
        # undefStatement
    |   statement statementModifier NL* expressionOrCommand
        # modifierStatement
    |   singleAssignmentStatement
        # singleAssignmentStatementStatement
    |   multipleAssignmentStatement
        # multipleAssignmentStatementStatement
    ;

definedMethodNameOrSymbol
    :   definedMethodName
    |   symbol
    ;

singleAssignmentStatement
    :   variable assignmentOperator NL* methodInvocationWithoutParentheses
    |   COLON2 CONSTANT_IDENTIFIER assignmentOperator NL* methodInvocationWithoutParentheses
    |   primary LBRACK indexingArgumentList? RBRACK assignmentOperator NL* methodInvocationWithoutParentheses
    |   primary (DOT | COLON2) methodName assignmentOperator NL* methodInvocationWithoutParentheses
    ;

multipleAssignmentStatement
    :   leftHandSide EQ NL* multipleRightHandSide
    |   packingLeftHandSide EQ NL* (methodInvocationWithoutParentheses | operatorExpression)
    |   multipleLeftHandSide EQ NL* multipleRightHandSide
    |   multipleLeftHandSideExceptPacking EQ NL* (methodInvocationWithoutParentheses | operatorExpression)
    ;

leftHandSide
    :   variable (EQ primary)?
        # variableLeftHandSide
    |   primary LBRACK indexingArgumentList? RBRACK
        # indexingLeftHandSide
    |   primary (DOT | COLON2) (LOCAL_VARIABLE_IDENTIFIER | CONSTANT_IDENTIFIER)
        # memberAccessLeftHandSide
    |   COLON2 CONSTANT_IDENTIFIER
        # qualifiedLeftHandSide
    ;

multipleLeftHandSide
    :   (multipleLeftHandSideItem COMMA)+ multipleLeftHandSideItem?
    |   (multipleLeftHandSideItem COMMA)+ packingLeftHandSide? (COMMA? NL* procParameter)? COMMA?
    |   packingLeftHandSide
    |   groupedLeftHandSide
    ;

multipleLeftHandSideExceptPacking
    :   (multipleLeftHandSideItem COMMA)+ multipleLeftHandSideItem?
    |   (multipleLeftHandSideItem COMMA)+ packingLeftHandSide?
    |   groupedLeftHandSide
    ;

packingLeftHandSide
    :   STAR leftHandSide?
    |   STAR leftHandSide (COMMA multipleLeftHandSideItem)*
    ;

groupedLeftHandSide
    :   LPAREN multipleLeftHandSide RPAREN
    ;

multipleLeftHandSideItem
    :   leftHandSide
    |   groupedLeftHandSide
    ;

multipleRightHandSide
    :   operatorExpressionList (COMMA splattingRightHandSide)?
    |   splattingRightHandSide
    ;
     
splattingRightHandSide
    :   splattingArgument
    ;

// --------------------------------------------------------
// Method invocation expressions
// --------------------------------------------------------

methodIdentifier
    :   LOCAL_VARIABLE_IDENTIFIER
    |   CONSTANT_IDENTIFIER
    |   methodOnlyIdentifier
    ;

methodName
    :   methodIdentifier
    |   keyword
    |   pseudoVariable
    ;

methodOnlyIdentifier
    :   (CONSTANT_IDENTIFIER | LOCAL_VARIABLE_IDENTIFIER | pseudoVariable) (EMARK | QMARK | EQ)
    ;
    
methodInvocationWithoutParentheses
    :   command
        # commandMethodInvocationWithoutParentheses
    |   chainedCommandWithDoBlock ((DOT | COLON2) methodName commandArgumentList)?
        # chainedMethodInvocationWithoutParentheses
    |   RETURN primaryValueList
        # returnMethodInvocationWithoutParentheses
    |   BREAK primaryValueList
        # breakMethodInvocationWithoutParentheses
    |   NEXT primaryValueList
        # nextMethodInvocationWithoutParentheses
    |   YIELD primaryValueList
        # yieldMethodInvocationWithoutParentheses
    ;

command
    :   primary NL? (AMPDOT | DOT | COLON2) methodName commandArgument
        # memberAccessCommand
    |   methodIdentifier commandArgument
        # simpleCommand
    ;

commandArgument
    :   commandArgumentList
        # commandArgumentCommandArgumentList
    |   command
        # commandCommandArgumentList
    ;

chainedCommandWithDoBlock
    :   commandWithDoBlock chainedMethodInvocation*
    ;

chainedMethodInvocation
    :   (DOT | COLON2) methodName argumentWithParentheses?
    ;

commandWithDoBlock
    :   SUPER argumentList doBlock
    |   methodIdentifier argumentList doBlock
    |   primary (DOT | COLON2) methodName argumentList doBlock
    ;

indexingArgumentList
    :   command
        # commandIndexingArgumentList
    |   operatorExpressionList COMMA?
        # operatorExpressionListIndexingArgumentList
    |   operatorExpressionList COMMA splattingArgument
        # operatorExpressionListWithSplattingArgumentIndexingArgumentList
    |   associationList COMMA?
        # associationListIndexingArgumentList
    |   splattingArgument
        # splattingArgumentIndexingArgumentList
    ;

splattingArgument
    :   STAR operatorExpression
    |   STAR2 operatorExpression
    ;

operatorExpressionList
    :   operatorExpression (COMMA NL* operatorExpression)*
    ;
    
operatorExpressionList2
    :   operatorExpression (COMMA NL* operatorExpression)+
    ;

argumentWithParentheses
    :   LPAREN NL* COMMA? NL* RPAREN
        # emptyArgumentWithParentheses
    |   LPAREN NL* argumentList COMMA? NL* RPAREN
        # argumentListArgumentWithParentheses
    |   LPAREN NL* operatorExpressionList COMMA NL* chainedCommandWithDoBlock COMMA? NL* RPAREN
        # operatorExpressionsAndChainedCommandWithBlockArgumentWithParentheses
    |   LPAREN NL* chainedCommandWithDoBlock COMMA? NL* RPAREN
        # chainedCommandWithDoBlockArgumentWithParentheses
    ;

argumentList
    :   blockArgument
        # blockArgumentArgumentList
    |   splattingArgument (COMMA NL* blockArgument)?
        # splattingArgumentArgumentList
    |   operatorExpressionList (COMMA NL* associationList)? (COMMA NL* splattingArgument)? (COMMA NL* blockArgument)?
        # operatorsArgumentList
    |   associationList (COMMA NL* splattingArgument)? (COMMA NL* blockArgument)?
        # associationsArgumentList
    |   command
        # singleCommandArgumentList
    ;
    
commandArgumentList
    :   associationList
    |   primaryValueList (COMMA NL* associationList)?
    ;    

primaryValueList
    :   primaryValue (COMMA NL* primaryValue)*
    ;

blockArgument
    :   AMP operatorExpression
    ;
    
// --------------------------------------------------------
// Expressions
// --------------------------------------------------------

expressionOrCommand
    :   operatorExpression
        # operatorExpressionOrCommand
    |   EMARK? methodInvocationWithoutParentheses
        # commandExpressionOrCommand
    |   NOT NL* expressionOrCommand
        # notExpressionOrCommand
    |   lhs=expressionOrCommand binOp=(AND|OR) NL* rhs=expressionOrCommand
        # keywordAndOrExpressionOrCommand
    ;

operatorExpression
    :   primary
        # primaryOperatorExpression
    |   operatorExpression QMARK NL* operatorExpression NL* COLON NL* operatorExpression
        # ternaryOperatorExpression
    ;

primary
    :   RETURN
        # returnWithoutArguments
    |   BREAK
        # breakWithoutArguments
    |   NEXT
        # nextWithoutArguments
    |   REDO
        # redoWithoutArguments
    |   RETRY
        # retryWithoutArguments
    |   primaryValue
        # primaryValuePrimary
    ;

primaryValue
    :   // Assignment expressions
        lhs=variable assignmentOperator NL* rhs=operatorExpression
        # localVariableAssignmentExpression
    |   primaryValue op=(DOT | COLON2) methodName assignmentOperator NL* operatorExpression
        # attributeAssignmentExpression
    |   COLON2 CONSTANT_IDENTIFIER assignmentOperator NL* operatorExpression
        # constantAssignmentExpression
    |   primaryValue LBRACK indexingArgumentList? RBRACK assignmentOperator NL* operatorExpression
        # bracketAssignmentExpression
    |   primaryValue assignmentOperator NL* operatorExpression RESCUE operatorExpression
        # assignmentWithRescue
        
        // Definitions
    |   CLASS classPath (LT commandOrPrimaryValueClass)? (SEMI | NL) bodyStatement END
        # classDefinition
    |   CLASS LT2 commandOrPrimaryValueClass (SEMI | NL) bodyStatement END
        # singletonClassDefinition
    |   MODULE classPath bodyStatement END
        # moduleDefinition
    |   DEF definedMethodName methodParameterPart bodyStatement END
        # methodDefinition
    |   DEF singletonObject op=(DOT | COLON2) definedMethodName methodParameterPart bodyStatement END
        # singletonMethodDefinition
    |   DEF definedMethodName (LPAREN parameterList? RPAREN)? EQ NL* statement
        # endlessMethodDefinition
    |   MINUSGT (LPAREN parameterList? RPAREN)? block
        # lambdaExpression

        // Control structures
    |   IF NL* expressionOrCommand thenClause elsifClause* elseClause? END
        # ifExpression
    |   UNLESS NL* expressionOrCommand thenClause elseClause? END
        # unlessExpression
    |   UNTIL NL* expressionOrCommand doClause END
        # untilExpression
    |   YIELD argumentWithParentheses?
        # yieldExpression
    |   BEGIN bodyStatement END
        # beginEndExpression
    |   CASE NL* expressionOrCommand (SEMI | NL)* whenClause+ elseClause? END
        # caseWithExpression
    |   CASE (SEMI | NL)* whenClause+ elseClause? END
        # caseWithoutExpression
    |   WHILE NL* expressionOrCommand doClause END
        # whileExpression
    |   FOR NL* forVariable IN NL* commandOrPrimaryValue doClause END
        # forExpression
    
        // Non-nested calls
    |   SUPER argumentWithParentheses? block?
        # superWithParentheses
    |   SUPER argumentList? block?
        # superWithoutParentheses
    |   isDefinedKeyword LPAREN expressionOrCommand RPAREN
        # isDefinedExpression
    |   isDefinedKeyword primaryValue
        # isDefinedCommand
    |   methodOnlyIdentifier
        # methodCallExpression
    |   methodIdentifier block
        # methodCallWithBlockExpression
    |   methodIdentifier argumentWithParentheses block?
        # methodCallWithParenthesesExpression
    |   variableReference
        # methodCallOrVariableReference
        
        // Literals
    |   LBRACK NL* indexingArgumentList? NL* RBRACK
        # bracketedArrayLiteral
    |   QUOTED_NON_EXPANDED_STRING_ARRAY_LITERAL_START quotedNonExpandedArrayElementList? QUOTED_NON_EXPANDED_STRING_ARRAY_LITERAL_END
        # quotedNonExpandedStringArrayLiteral
    |   QUOTED_NON_EXPANDED_SYMBOL_ARRAY_LITERAL_START quotedNonExpandedArrayElementList? QUOTED_NON_EXPANDED_SYMBOL_ARRAY_LITERAL_END
        # quotedNonExpandedSymbolArrayLiteral
    |   QUOTED_EXPANDED_STRING_ARRAY_LITERAL_START quotedExpandedArrayElementList? QUOTED_EXPANDED_STRING_ARRAY_LITERAL_END
        # quotedExpandedStringArrayLiteral
    |   QUOTED_EXPANDED_SYMBOL_ARRAY_LITERAL_START quotedExpandedArrayElementList? QUOTED_EXPANDED_SYMBOL_ARRAY_LITERAL_END
        # quotedExpandedSymbolArrayLiteral
    |   LCURLY NL* (associationList COMMA?)? NL* RCURLY
        # hashLiteral
    |   sign=(PLUS | MINUS)? unsignedNumericLiteral
        # numericLiteral
    |   singleQuotedString singleOrDoubleQuotedString*
        # singleQuotedStringExpression
    |   doubleQuotedString singleOrDoubleQuotedString*
        # doubleQuotedStringExpression
    |   QUOTED_NON_EXPANDED_STRING_LITERAL_START NON_EXPANDED_LITERAL_CHARACTER_SEQUENCE? QUOTED_NON_EXPANDED_STRING_LITERAL_END
        # quotedNonExpandedStringLiteral
    |   QUOTED_EXPANDED_STRING_LITERAL_START quotedExpandedLiteralStringContent* QUOTED_EXPANDED_STRING_LITERAL_END
        # quotedExpandedStringLiteral
    |   QUOTED_EXPANDED_EXTERNAL_COMMAND_LITERAL_START quotedExpandedLiteralStringContent* QUOTED_EXPANDED_EXTERNAL_COMMAND_LITERAL_END
        # quotedExpandedExternalCommandLiteral
    |   symbol
        # symbolExpression
    |   REGULAR_EXPRESSION_START regexpLiteralContent* REGULAR_EXPRESSION_END
        # regularExpressionLiteral
    |   QUOTED_EXPANDED_REGULAR_EXPRESSION_START quotedExpandedLiteralStringContent* QUOTED_EXPANDED_REGULAR_EXPRESSION_END
        # quotedExpandedRegularExpressionLiteral
    
    |   LPAREN compoundStatement RPAREN
        # groupingStatement
    
        // Member accesses
    |   primaryValue LBRACK indexingArgumentList? RBRACK
        # indexingAccessExpression
    |   primaryValue NL* op=(AMPDOT | DOT | COLON2) NL* methodName argumentWithParentheses? block?
        # memberAccessExpression
    
        // Unary and binary expressions
    |   unaryOperator primaryValue
        # unaryExpression
    |   <assoc=right> primaryValue powerOperator=STAR2    NL* primaryValue
        # powerExpression
    |   MINUS primaryValue
        # unaryMinusExpression
    |   primaryValue multiplicativeOperator NL* primaryValue
        # multiplicativeExpression
    |   primaryValue additiveOperator       NL* primaryValue
        # additiveExpression
    |   primaryValue bitwiseShiftOperator   NL* primaryValue
        # shiftExpression
    |   primaryValue bitwiseAndOperator=AMP NL* primaryValue
        # bitwiseAndExpression
    |   primaryValue bitwiseOrOperator      NL* primaryValue
        # bitwiseOrExpression
    |   primaryValue relationalOperator     NL* primaryValue
        # relationalExpression
    |   primaryValue equalityOperator       NL* primaryValue
        # equalityExpression
    |   primaryValue andOperator=AMP2       NL* primaryValue
        # logicalAndExpression
    |   primaryValue orOperator=BAR2        NL* primaryValue
        # logicalOrExpression
    |   primaryValue rangeOperator          NL* primaryValue
        # rangeExpression
    |   hereDoc
        # hereDocs
    ;

// This is required to make chained calls work. For classes, we cannot move up the `primaryValue` due to the possible
// presence of AMPDOT when inheriting (class Foo < Bar::Baz), but the command rule doesn't allow chained calls
// in if statements to be created properly, and ends throwing away everything after the first call. Splitting these
// allows us to have a rule for the class that parses properly, and a rule for everything else that allows us to move
// up the `primaryValue` rule to the top.
commandOrPrimaryValueClass
    :   command
        # commandCommandOrPrimaryValueClass
    |   primaryValue
        # primaryValueCommandOrPrimaryValueClass
    ;

commandOrPrimaryValue
    :   primaryValue
        # primaryValueCommandOrPrimaryValue
    |   command
        # commandCommandOrPrimaryValue
    |   NOT commandOrPrimaryValue
        # notCommandOrPrimaryValue
    |   commandOrPrimaryValue (AND|OR) NL* commandOrPrimaryValue
        # keywordAndOrCommandOrPrimaryValue
    ;

block
    :   LCURLY NL* blockParameter? compoundStatement RCURLY
        # curlyBracesBlock
    |   doBlock
        # doBlockBlock
    ;

doBlock
    :   DO NL* blockParameter? bodyStatement END
    ;

blockParameter
    :   BAR NL* BAR
    |   BAR NL* parameterList NL* BAR
    ;

thenClause
    :   (SEMI | NL)+ compoundStatement
    |   (SEMI | NL)? THEN compoundStatement
    ;

elseClause
    :   ELSE compoundStatement
    ;

elsifClause
    :   ELSIF NL* expressionOrCommand thenClause
    ;

whenClause
    :   WHEN NL* whenArgument thenClause
    ;

whenArgument
    :   operatorExpressionList (COMMA splattingArgument)?
    |   splattingArgument
    ;

doClause
    :   (SEMI | NL)+ compoundStatement
    |   DO compoundStatement
    ;

forVariable
    :   leftHandSide
    |   multipleLeftHandSide
    ;

bodyStatement
    :   compoundStatement rescueClause* elseClause? ensureClause?
    ;

rescueClause
    :   RESCUE exceptionClassList? exceptionVariableAssignment? thenClause
    ;

exceptionClassList
    :   operatorExpression
    |   multipleRightHandSide
    ;

exceptionVariableAssignment
    :   EQGT leftHandSide
    ;
    
ensureClause
    :   ENSURE compoundStatement
    ;

definedMethodName
    :   methodName
    |   ASSIGNMENT_LIKE_METHOD_IDENTIFIER
    |   LBRACK RBRACK EQ?
    |   EQ2
    |   EQ3
    |   LTEQGT
    |   LT2
    ;

methodParameterPart
    :   LPAREN NL* parameterList? NL* RPAREN
    |   parameterList? (SEMI | NL)
    ;

parameterList
    :   mandatoryOrOptionalParameterList (COMMA NL* arrayParameter)? (COMMA NL* hashParameter)? (COMMA NL* procParameter)?
    |   arrayParameter (COMMA NL* hashParameter)? (COMMA NL* procParameter)?
    |   hashParameter (COMMA NL* procParameter)?
    |   procParameter
    ;

mandatoryOrOptionalParameterList
    :   mandatoryOrOptionalParameter (COMMA NL* mandatoryOrOptionalParameter)*
    ;
    
mandatoryOrOptionalParameter
    :   mandatoryParameter
        # mandatoryMandatoryOrOptionalParameter
    |   optionalParameter
        # optionalMandatoryOrOptionalParameter
    ;

mandatoryParameter
    :   LOCAL_VARIABLE_IDENTIFIER COLON?
    ;

optionalParameter
    :   optionalParameterName (EQ|COLON) NL* operatorExpression
    ;

optionalParameterName
    :   LOCAL_VARIABLE_IDENTIFIER
    ;

arrayParameter
    :   STAR LOCAL_VARIABLE_IDENTIFIER?
    ;

hashParameter
    :   STAR2 LOCAL_VARIABLE_IDENTIFIER?
    ;

procParameter
    :   AMP procParameterName
    ;

procParameterName
    :   LOCAL_VARIABLE_IDENTIFIER
    ;

classPath
    :   COLON2 CONSTANT_IDENTIFIER
        # topClassPath
    |   CONSTANT_IDENTIFIER
        # className
    |   classPath COLON2 CONSTANT_IDENTIFIER
        # nestedClassPath
    ;

singletonObject
    :   variableReference
        #variableReferenceSingletonObject
    |   LPAREN expressionOrCommand RPAREN
        #expressionSingletonObject
    ;

variableReference
    :   variable
        # variableVariableReference
    |   pseudoVariable
        # pseudoVariableVariableReference
    |   COLON2 CONSTANT_IDENTIFIER
        # constantVariableReference
    ;

associationList
    :   association (COMMA NL* association)*
    ;
    
association
    :   associationKey (EQGT | COLON) NL* operatorExpression
    ;
    
associationKey
    :   operatorExpression
    |   keyword
    ;

regexpLiteralContent
    :   REGULAR_EXPRESSION_BODY
    |   REGULAR_EXPRESSION_INTERPOLATION_BEGIN compoundStatement REGULAR_EXPRESSION_INTERPOLATION_END
    ;

singleQuotedString
    :   SINGLE_QUOTED_STRING_LITERAL
    ;

singleOrDoubleQuotedString
    :   singleQuotedString
    |   doubleQuotedString
    ;

doubleQuotedString
    :   DOUBLE_QUOTED_STRING_START doubleQuotedStringContent* DOUBLE_QUOTED_STRING_END
    ;

quotedExpandedExternalCommandString
    :   QUOTED_EXPANDED_EXTERNAL_COMMAND_LITERAL_START 
        quotedExpandedLiteralStringContent*
        QUOTED_EXPANDED_EXTERNAL_COMMAND_LITERAL_END
    ;

doubleQuotedStringContent
    :   DOUBLE_QUOTED_STRING_CHARACTER_SEQUENCE
    |   STRING_INTERPOLATION_BEGIN compoundStatement STRING_INTERPOLATION_END
    ;

quotedNonExpandedLiteralString
    :   QUOTED_NON_EXPANDED_STRING_LITERAL_START NON_EXPANDED_LITERAL_CHARACTER_SEQUENCE? QUOTED_NON_EXPANDED_STRING_LITERAL_END
    ;

quotedExpandedLiteralString
    :   QUOTED_EXPANDED_STRING_LITERAL_START quotedExpandedLiteralStringContent* QUOTED_EXPANDED_STRING_LITERAL_END
    ;

quotedExpandedLiteralStringContent
    :   EXPANDED_LITERAL_CHARACTER_SEQUENCE
    |   DELIMITED_STRING_INTERPOLATION_BEGIN compoundStatement DELIMITED_STRING_INTERPOLATION_END
    ;

quotedNonExpandedArrayElementContent
    :   NON_EXPANDED_ARRAY_ITEM_CHARACTER+
    ;
    
quotedExpandedArrayElementContent
    :   EXPANDED_ARRAY_ITEM_CHARACTER
    |   DELIMITED_ARRAY_ITEM_INTERPOLATION_BEGIN compoundStatement DELIMITED_ARRAY_ITEM_INTERPOLATION_END
    ;

quotedExpandedArrayElement
    :   quotedExpandedArrayElementContent+
    ;

quotedNonExpandedArrayElementList
    :   NON_EXPANDED_ARRAY_ITEM_SEPARATOR*
        quotedNonExpandedArrayElementContent 
        (NON_EXPANDED_ARRAY_ITEM_SEPARATOR+ quotedNonExpandedArrayElementContent)*
        NON_EXPANDED_ARRAY_ITEM_SEPARATOR*
    ;

quotedExpandedArrayElementList
    :   EXPANDED_ARRAY_ITEM_SEPARATOR*
        quotedExpandedArrayElement
        (EXPANDED_ARRAY_ITEM_SEPARATOR+ quotedExpandedArrayElement)*
        EXPANDED_ARRAY_ITEM_SEPARATOR*
    ;

symbol
    :   SYMBOL_LITERAL
        # pureSymbolLiteral
    |   COLON singleQuotedString
        # singleQuotedSymbolLiteral
    |   COLON doubleQuotedString
        # doubleQuotedSymbolLiteral
    ;

hereDoc
    : HERE_DOC
    ;

// --------------------------------------------------------
// Commons
// --------------------------------------------------------

isDefinedKeyword
    :   IS_DEFINED
    ;

assignmentOperator
    :   EQ
    |   ASSIGNMENT_OPERATOR
    ;

statementModifier
    :   IF
    |   UNLESS
    |   WHILE
    |   UNTIL
    |   RESCUE
    ;

variable
    :   CONSTANT_IDENTIFIER
        # constantIdentifierVariable
    |   GLOBAL_VARIABLE_IDENTIFIER
        # globalIdentifierVariable
    |   CLASS_VARIABLE_IDENTIFIER
        # classIdentifierVariable
    |   INSTANCE_VARIABLE_IDENTIFIER
        # instanceIdentifierVariable
    |   LOCAL_VARIABLE_IDENTIFIER
        # localIdentifierVariable
    ;

pseudoVariable
    :   NIL
        # nilPseudoVariable
    |   TRUE
        # truePseudoVariable
    |   FALSE
        # falsePseudoVariable
    |   SELF
        # selfPseudoVariable
    |   LINE__
        # linePseudoVariable
    |   FILE__
        # filePseudoVariable
    |   ENCODING__
        # encodingPseudoVariable
    ;

unsignedNumericLiteral
    :   DECIMAL_INTEGER_LITERAL
        # decimalUnsignedLiteral
    |   BINARY_INTEGER_LITERAL
        # binaryUnsignedLiteral
    |   OCTAL_INTEGER_LITERAL
        # octalUnsignedLiteral
    |   HEXADECIMAL_INTEGER_LITERAL
        # hexadecimalUnsignedLiteral
    |   FLOAT_LITERAL_WITHOUT_EXPONENT
        # floatWithoutExponentUnsignedLiteral
    |   FLOAT_LITERAL_WITH_EXPONENT
        # floatWithExponentUnsignedLiteral
    ;

unaryOperator
    :   TILDE
    |   PLUS
    |   EMARK
    ;

multiplicativeOperator
    :   STAR
    |   SLASH
    |   PERCENT
    ;

additiveOperator
    :   PLUS
    |   MINUS
    ;

bitwiseShiftOperator
    :   LT2
    |   GT2
    ;

bitwiseOrOperator
    :   BAR
    |   CARET
    ;

relationalOperator
    :   GT
    |   GTEQ
    |   LT
    |   LTEQ
    ;

equalityOperator
    :   LTEQGT
    |   EQ2
    |   EQ3
    |   EMARKEQ
    |   EQTILDE
    |   EMARKTILDE
    ;

rangeOperator
    :   DOT2
    |   DOT3
    ;

keyword
    :   BEGIN_
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
    |   IF
    |   IN
    |   MODULE
    |   NEXT
    |   NOT
    |   OR
    |   REDO
    |   RESCUE
    |   RETRY
    |   RETURN
    |   SUPER
    |   THEN
    |   UNDEF
    |   UNLESS
    |   UNTIL
    |   WHEN
    |   WHILE
    |   YIELD
    ;