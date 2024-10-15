package io.joern.rubysrc2cpg.parser

import io.joern.rubysrc2cpg.astcreation.RubyIntermediateAst.{RubyExpression, TextSpan}
import upickle.core.*
import upickle.default.*

object RubyJsonHelpers {

  implicit class JsonObjHelper(o: ujson.Obj) {

    def toTextSpan: TextSpan = {
      val metaData =
        if (o.obj.contains(ParserKeys.MetaData)) read[MetaData](o(ParserKeys.MetaData))
        else read[MetaData](o)

      TextSpan(
        line = Option(metaData.lineNumber).filterNot(_ == -1),
        column = Option(metaData.columnNumber).filterNot(_ == -1),
        lineEnd = Option(metaData.lineNumberEnd).filterNot(_ == -1),
        columnEnd = Option(metaData.columnNumberEnd).filterNot(_ == -1),
        offset = Option(metaData.lineNumber, metaData.lineNumberEnd + 1),
        text = metaData.code
      )
    }

    def visitOption(key: String)(implicit visit: ujson.Value => RubyExpression): Option[RubyExpression] =
      if contains(key) then Option(visit(o(key))) else None

    def visitArray(key: String)(implicit visit: ujson.Value => RubyExpression): List[RubyExpression] = {
      o(key).arr.map(visit).toList
    }

    def contains(key: String): Boolean = o.obj.get(key).exists(x => x != null && x != ujson.Null)

  }

  private case class MetaData(
    code: String,
    @upickle.implicits.key("start_line") lineNumber: Int,
    @upickle.implicits.key("start_column") columnNumber: Int,
    @upickle.implicits.key("end_line") lineNumberEnd: Int,
    @upickle.implicits.key("end_column") columnNumberEnd: Int
  ) derives ReadWriter

}
