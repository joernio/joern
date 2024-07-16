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

  private def isPreviousTokenColonOrDot: Boolean = {
    val previousToken = previousTokenTypeOrEOF()
    previousToken == RubyLexer.DOT || previousToken == RubyLexer.COLON || previousToken == RubyLexer.COLON2
  }

  private def isNextTokenColonOrDot: Boolean = {
    _input.LA(1) == '.' || _input.LA(1) == ':'
  }

  def setKeywordTokenType(): Unit = {
    val tokenText = getText
    if (tokenText == null) {
      return
    }

    if (
      isPreviousTokenColonOrDot || (isNextTokenColonOrDot && tokenText.toUpperCase != "SELF") || !keywordMap.contains(
        tokenText.toUpperCase
      )
    ) {
      setType(RubyLexer.LOCAL_VARIABLE_IDENTIFIER)
    } else {
      keywordMap.get(tokenText.toUpperCase).foreach(setType)
    }
  }
}
