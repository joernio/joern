package io.joern.rubysrc2cpg.parser

import upickle.default.*

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
  val Type        = "type"
  val Value       = "value"
}

enum AstType(val name: String) {
  case Args   extends AstType("args")
  case Array  extends AstType("array")
  case Begin  extends AstType("begin")
  case CBase  extends AstType("cbase")
  case Class  extends AstType("class")
  case Const  extends AstType("const")
  case Def    extends AstType("def")
  case DStr   extends AstType("dstr")
  case DSym   extends AstType("dsym")
  case Hash   extends AstType("hash")
  case Int    extends AstType("int")
  case IVar   extends AstType("ivar")
  case LVAsgn extends AstType("lvasgn")
  case Pair   extends AstType("pair")
  case Send   extends AstType("send")
  case Splat  extends AstType("splat")
  case Str    extends AstType("str")
  case Sym    extends AstType("sym")
}

object AstType {
  def fromString(input: String): Option[AstType] = AstType.values.find(_.name == input)
}
