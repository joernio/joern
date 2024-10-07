package io.joern.rubysrc2cpg.parser

import org.slf4j.LoggerFactory
import upickle.core.*
import upickle.default.*

object RubyJsonAst {

  import AstTypes.*
  import ParserKeys.*

  private val logger = LoggerFactory.getLogger(this.getClass)

  sealed trait RubyExpression(metaData: MetaData) {
    def code: String         = metaData.code
    def lineNumber: Int      = metaData.lineNumber
    def columnNumber: Int    = metaData.columnNumber
    def lineNumberEnd: Int   = metaData.lineNumberEnd
    def columnNumberEnd: Int = metaData.columnNumberEnd
  }

  case class MetaData(
    code: String,
    @upickle.implicits.key("start_line") lineNumber: Int,
    @upickle.implicits.key("start_column") columnNumber: Int,
    @upickle.implicits.key("end_line") lineNumberEnd: Int,
    @upickle.implicits.key("end_column") columnNumberEnd: Int
  ) derives ReadWriter

  sealed trait RubyLiteral extends RubyExpression {
    def value: String
  }

  case class Unknown(metaData: MetaData) extends RubyExpression(metaData)

  case class NullResult() extends RubyExpression(MetaData("", -1, -1, -1, -1))

  case class RubyProgram(body: RubyExpression)

  case class RubyConst(metaData: MetaData, base: RubyExpression, name: String) extends RubyExpression(metaData)

  case class RubyBody(metaData: MetaData, statements: List[RubyExpression] = Nil) extends RubyExpression(metaData)

  case class RubyArray(metaData: MetaData, children: List[RubyExpression] = Nil) extends RubyExpression(metaData)

  case class RubyCall(metaData: MetaData, receiver: RubyExpression, name: String, arguments: List[RubyExpression])
      extends RubyExpression(metaData)

  case class RubyInt(metaData: MetaData, value: String) extends RubyExpression(metaData) with RubyLiteral

  case class RubyStr(metaData: MetaData, value: String) extends RubyExpression(metaData) with RubyLiteral

  implicit val rubyProgramRw: ReadWriter[RubyProgram] = readwriter[ujson.Value].bimap[RubyProgram](
    x => ujson.Null,
    {
      case x: ujson.Obj =>
        val body = visit(x)
        RubyProgram(body)
      case x => throw new RuntimeException(s"`RubyProgram` expects an object, not ${x.getClass}")
    }
  )

  private def visit(v: ujson.Value): RubyExpression = {
    v match {
      case obj: ujson.Obj => visit(obj)
      case ujson.Null     => NullResult()
      case x =>
        logger.warn(s"Unhandled ujson type ${x.getClass}")
        NullResult()
    }
  }

  private def visit(obj: ujson.Obj): RubyExpression = {
    val metaData = obj.toMetaData
    obj("type").str match {
      case Array => RubyArray(metaData, obj.visitArray(Children))
      case Begin => RubyBody(metaData, obj.visitArray(Body))
      case Const => RubyConst(metaData, visit(obj(Base)), obj(Name).str)
      case Int_  => RubyInt(metaData, obj(Value).num.toString)
      case Send  => RubyCall(metaData, visit(obj(Receiver)), obj(Name).str, obj.visitArray(Arguments))
      case Str   => RubyStr(metaData, obj(Value).str)
      case x =>
        logger.warn(s"Unhandled `parser` type '$x'")
        Unknown(metaData)
    }
  }

  implicit class jsonObjExt(o: ujson.Obj) {

    def toMetaData: MetaData = {
      if (o.obj.contains(ParserKeys.MetaData)) read[MetaData](o(ParserKeys.MetaData))
      else read[MetaData](o)
    }

    def visitArray(key: String): List[RubyExpression] = {
      o(key).arr.map(visit).toList
    }

  }

}

/** The JSON key values, in alphabetical order.
  */
object ParserKeys {
  val Children    = "children"
  val FilePath    = "file_path"
  val MetaData    = "meta_data"
  val RelFilePath = "rel_file_path"
  val Value       = "value"
}

/** The JSON node types.
  */
object AstTypes {
  val Arguments = "arguments"
  val Array     = "array"
  val Base      = "base"
  val Begin     = "begin"
  val Body      = "body"
  val Const     = "const"
  val Int_      = "int"
  val Name      = "name"
  val Receiver  = "receiver"
  val Send      = "send"
  val Str       = "str"
}
