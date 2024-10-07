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

  private def emptyMetaData = MetaData("", -1, -1, -1, -1)

  // Special nodes

  case class Unknown(metaData: MetaData) extends RubyExpression(metaData)

  case class NullResult() extends RubyExpression(emptyMetaData)

  case class RubyProgram(body: RubyExpression)

  // Body nodes

  case class RubyArgs(metaData: MetaData, children: List[RubyExpression] = Nil) extends RubyExpression(metaData)

  case class RubyArray(metaData: MetaData, children: List[RubyExpression] = Nil) extends RubyExpression(metaData)

  case class RubyBody(metaData: MetaData, statements: List[RubyExpression] = Nil) extends RubyExpression(metaData)

  case class RubyConstantBase(metaData: MetaData, base: RubyExpression, name: RubyExpression)
      extends RubyExpression(metaData)

  case class RubyCall(metaData: MetaData, receiver: RubyExpression, name: String, arguments: List[RubyExpression])
      extends RubyExpression(metaData)

  case class RubyClass(
    metaData: MetaData,
    name: RubyExpression,
    superclass: Option[RubyExpression],
    body: RubyExpression
  ) extends RubyExpression(metaData)

  case class RubyConst(metaData: MetaData, base: RubyExpression, name: String) extends RubyExpression(metaData)

  case class RubyDynamicStr(metaData: MetaData, children: List[RubyExpression]) extends RubyExpression(metaData)

  case class RubyDynamicSym(metaData: MetaData, children: List[RubyExpression]) extends RubyExpression(metaData)

  case class RubyHash(metaData: MetaData, children: List[RubyExpression]) extends RubyExpression(metaData)

  case class RubyInt(metaData: MetaData, value: String) extends RubyExpression(metaData) with RubyLiteral

  case class RubyIVar(metaData: MetaData, value: String) extends RubyExpression(metaData)

  case class RubyLeftVariableAssign(metaData: MetaData, lhs: RubyExpression, rhs: RubyExpression)
      extends RubyExpression(metaData)

  case class RubyMethod(metaData: MetaData, name: String, parameters: RubyExpression, body: RubyExpression)
      extends RubyExpression(metaData)

  case class RubyPair(metaData: MetaData, key: RubyExpression, value: RubyExpression) extends RubyExpression(metaData)

  case class RubySplat(metaData: MetaData, value: RubyExpression) extends RubyExpression(metaData)

  case class RubyStr(metaData: MetaData, value: String) extends RubyExpression(metaData) with RubyLiteral

  case class RubySymbol(metaData: MetaData, value: String) extends RubyExpression(metaData) with RubyLiteral

  implicit val rubyProgramRw: ReadWriter[RubyProgram] = readwriter[ujson.Value].bimap[RubyProgram](
    _ => ujson.Null,
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
      case ujson.Str(x)   => RubyStr(emptyMetaData.copy(code = x), x)
      case x =>
        logger.warn(s"Unhandled ujson type ${x.getClass}")
        NullResult()
    }
  }

  private def visit(obj: ujson.Obj): RubyExpression = {
    val metaData = obj.toMetaData
    obj("type").str match {
      case Args  => RubyArgs(metaData, obj.visitArray(Children))
      case Array => RubyArray(metaData, obj.visitArray(Children))
      case Begin => RubyBody(metaData, obj.visitArray(Body))
      case CBase => RubyConstantBase(metaData, visit(obj(Base)), visit(obj(Name)))
      case Class =>
        val superClass = if obj.obj.contains(SuperClass) then Option(visit(obj(SuperClass))) else None
        RubyClass(metaData, visit(obj(Name)), superClass, visit(obj(Body)))
      case Const  => RubyConst(metaData, visit(obj(Base)), obj(Name).str)
      case Def    => RubyMethod(metaData, obj(Name).str, visit(obj(Arguments)), visit(obj(Body)))
      case DStr   => RubyDynamicStr(metaData, obj.visitArray(Children))
      case DSym   => RubyDynamicSym(metaData, obj.visitArray(Children))
      case Hash   => RubyHash(metaData, obj.visitArray(Children))
      case Int_   => RubyInt(metaData, obj(Value).num.toString)
      case IVar   => RubyIVar(metaData, obj(Value).str)
      case LVAsgn => RubyLeftVariableAssign(metaData, visit(obj(Lhs)), visit(obj(Rhs)))
      case Pair   => RubyPair(metaData, visit(obj(Key)), visit(obj(Value)))
      case Send   => RubyCall(metaData, visit(obj(Receiver)), obj(Name).str, obj.visitArray(Arguments))
      case Splat  => RubySplat(metaData, visit(obj(Value)))
      case Str    => RubyStr(metaData, obj(Value).str)
      case Sym    => RubySymbol(metaData, obj(Value).str)
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
  val Arguments   = "arguments"
  val Base        = "base"
  val Body        = "body"
  val Children    = "children"
  val FilePath    = "file_path"
  val Key         = "key"
  val Lhs         = "lhs"
  val MetaData    = "meta_data"
  val Name        = "name"
  val RelFilePath = "rel_file_path"
  val Receiver    = "receiver"
  val Rhs         = "rhs"
  val SuperClass  = "superclass"
  val Value       = "value"
}

/** The JSON node types.
  */
object AstTypes {
  val Args   = "args"
  val Array  = "array"
  val Begin  = "begin"
  val CBase  = "cbase"
  val Class  = "class"
  val Const  = "const"
  val Def    = "def"
  val DStr   = "dstr"
  val DSym   = "dsym"
  val Hash   = "hash"
  val Int_   = "int"
  val IVar   = "ivar"
  val LVAsgn = "lvasgn"
  val Pair   = "pair"
  val Send   = "send"
  val Splat  = "splat"
  val Str    = "str"
  val Sym    = "sym"
}
