package io.joern.rubysrc2cpg.parser

import io.joern.rubysrc2cpg.parser.RubyLexer.*

trait KeywordHandling { this: RubyLexerBase =>

  val keywordMap: Map[String, Int] = Map(
    "LINE__"     -> LINE__,
    "ENCODING__" -> ENCODING__,
    "FILE__"     -> FILE__,
    "BEGIN_"     -> BEGIN_,
    "END_"       -> END_,
    "ALIAS"      -> ALIAS,
    "AND"        -> AND,
    "BEGIN"      -> BEGIN,
    "BREAK"      -> BREAK,
    "CASE"       -> CASE,
    "CLASS"      -> CLASS,
    "DEF"        -> DEF,
    "IS_DEFINED" -> IS_DEFINED,
    "DO"         -> DO,
    "ELSE"       -> ELSE,
    "ELSIF"      -> ELSIF,
    "END"        -> END,
    "ENSURE"     -> ENSURE,
    "FOR"        -> FOR,
    "FALSE"      -> FALSE,
    "IF"         -> IF,
    "IN"         -> IN,
    "MODULE"     -> MODULE,
    "NEXT"       -> NEXT,
    "NIL"        -> NIL,
    "NOT"        -> NOT,
    "OR"         -> OR,
    "REDO"       -> REDO,
    "RESCUE"     -> RESCUE,
    "RETRY"      -> RETRY,
    "RETURN"     -> RETURN,
    "SELF"       -> SELF,
    "SUPER"      -> SUPER,
    "THEN"       -> THEN,
    "TRUE"       -> TRUE,
    "UNDEF"      -> UNDEF,
    "UNLESS"     -> UNLESS,
    "UNTIL"      -> UNTIL,
    "WHEN"       -> WHEN,
    "WHILE"      -> WHILE,
    "YIELD"      -> YIELD
  )

  def setKeywordTokenType(): Unit = {
    if (_token == null) {
      return
    }

    if (previousNonWsTokenTypeOrEOF() == RubyLexer.DOT || !keywordMap.contains(_token.getText)) {
      setType(RubyLexer.LOCAL_VARIABLE_IDENTIFIER)
    } else {
      keywordMap.get(_token.getText).foreach(setType)
    }
  }
}
