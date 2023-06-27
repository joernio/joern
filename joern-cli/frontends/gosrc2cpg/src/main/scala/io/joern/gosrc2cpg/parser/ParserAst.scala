package io.joern.gosrc2cpg.parser

object ParserAst {

  private val QualifiedClassName: String = ParserAst.getClass.getName

  def fromString(nodeName: String): ParserNode = {
    val clazz = Class.forName(s"$QualifiedClassName${nodeName.stripPrefix("ast.")}$$")
    clazz.getField("MODULE$").get(clazz).asInstanceOf[ParserNode]
  }

  sealed trait ParserNode {
    override def toString: String = this.getClass.getSimpleName.stripSuffix("$")
  }

  object GenDecl    extends ParserNode
  object ImportSpec extends ParserNode
  object BasicLit   extends ParserNode

  object FuncDecl extends ParserNode

}

object ParserKeys {

  val Path         = "Path"
  val Value        = "Value"
  val Name         = "Name"
  val Specs        = "Specs"
  val Decls        = "Decls"
  val NodeFileName = "node_filename"
  val NodeType     = "node_type"
  val NodeLineNo   = "node_line_no"
  val NodeColNo    = "node_col_no"
}
