grammar Semantics;

taintSemantics: singleSemantic* EOF;
singleSemantic: methodName mapping* NEWLINE*;
methodName : QUOTE name QUOTE;
name : ~(NEWLINE|QUOTE)*?;

mapping: PASSTHROUGH | (src '->' dst);
argName: QUOTE name QUOTE;
argIdx: NUMBER;
src: argIdx (argName)?;
dst: argIdx (argName)?;

// Keywords

PASSTHROUGH: 'PASSTHROUGH';

// Lexing

QUOTE : '"';
NUMBER: [-]?[0-9]+;
NEWLINE          : '\r'? '\n';
LINE_COMMENT : '#' .*? ('\n'|EOF)	-> channel(HIDDEN) ;
WHITESPACE : [ \r\t\u000C\n]+ -> skip;
OTHER: .;
