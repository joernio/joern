grammar Semantics;

taintSemantics: singleSemantic* EOF;
singleSemantic: methodName mapping* NEWLINE*;
methodName : QUOTE name QUOTE;
name : ~(NEWLINE|QUOTE)*?;

mapping: src '->' dst;
argName: QUOTE name QUOTE;
argIdx: NUMBER;
src: argName | argIdx;
dst: argName | argIdx;

// Lexing

 QUOTE : '"';
NUMBER: [-]?[0-9]+;
NEWLINE          : '\r'? '\n';
LINE_COMMENT : '#' .*? ('\n'|EOF)	-> channel(HIDDEN) ;
WHITESPACE : [ \r\t\u000C\n]+ -> skip;
OTHER: .;
