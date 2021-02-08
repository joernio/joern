package io.shiftleft.pythonparser.ast


import java.util
import scala.jdk.CollectionConverters._

trait istmt extends iast with iattributes

case class ErrorStatement(lineno: Int, col_offset: Int) extends istmt

case class Module(stmts: Iterable[istmt]) extends imod {
  def this(stmts: util.ArrayList[istmt]) = {
    this(stmts.asScala)
  }

  override def print: String = {
    stmts.map(_.print).mkString("\n")
  }
}

case class Assign(targets: Iterable[iexpr],
                  value: iexpr,
                  typeComment: Option[String],
                  lineno: Int,
                  col_offset: Int) extends istmt {
  def this(targets: util.ArrayList[iexpr],
           value: iexpr,
           typeComment: Option[String],
           attributeProvider: AttributeProvider) = {
    this(targets.asScala, value, typeComment, attributeProvider.lineno, attributeProvider.col_offset)
  }
  def this(targets: util.ArrayList[iexpr],
           value: iexpr,
           attributeProvider: AttributeProvider) = {
    this(targets.asScala, value, None, attributeProvider.lineno, attributeProvider.col_offset)
  }
  override def print: String = {
    targets.map(_.print).mkString(", ") + " = " + value.print
  }
}

case class AnnAssign(target: iexpr,
                     annotation: iexpr,
                     value: Option[iexpr],
                     simple: Boolean,
                     lineno: Int,
                     col_offset: Int) extends istmt {
  def this(target: iexpr,
           annotation: iexpr,
           simple: Boolean,
           attributeProvider: AttributeProvider) = {
    this(target, annotation, None, simple, attributeProvider.lineno, attributeProvider.col_offset)
  }
  def this(target: iexpr,
           annotation: iexpr,
           value: iexpr,
           simple: Boolean,
           attributeProvider: AttributeProvider) = {
    this(target, annotation, Some(value), simple, attributeProvider.lineno, attributeProvider.col_offset)
  }
}

case class AugAssign(target: iexpr,
                     op: ioperator,
                     value: iexpr,
                     lineno: Int,
                     col_offset: Int) extends istmt {
  def this(target: iexpr,
           op: ioperator,
           value: iexpr,
           attributeProvider: AttributeProvider) = {
    this(target, op, value, attributeProvider.lineno, attributeProvider.col_offset)
  }
}

case class Expr(value: iexpr, lineno: Int, col_offset: Int) extends istmt {
  def this(value: iexpr) = {
    this(value, value.lineno, value.col_offset)
  }
  override def print: String = {
    value.print
  }
}

case class Return(value: Option[iexpr], lineno: Int, col_offset: Int) extends istmt {
  def this(value: iexpr, attributeProvider: AttributeProvider) = {
    this(Option(value), attributeProvider.lineno, attributeProvider.col_offset)
  }
  override def print: String = {
    "return" + value.map(v => " " + v.print).getOrElse("")
  }
}

case class Import(names: Iterable[ialias], lineno: Int, col_offset: Int) extends istmt {
  def this(names: util.ArrayList[ialias], attributeProvider: AttributeProvider) = {
    this(names.asScala, attributeProvider.lineno, attributeProvider.col_offset)
  }
  override def print: String = {
    "import " + names.map(_.print).mkString(", ")
  }
}

case class ImportFrom(module: Option[String],
                      names: Iterable[ialias],
                      level: Int,
                      lineno: Int,
                      col_offset: Int) extends istmt {
  def this(module: String, names: util.ArrayList[ialias], level: Int, attributeProvider: AttributeProvider) = {
    this(Option(module), names.asScala, level, attributeProvider.lineno, attributeProvider.col_offset)
  }
  override def print: String = {
    val relativeImportDots =
      if (level != 0) {
        " " + "." * level
      } else {
        ""
      }
    "from" + relativeImportDots + module.map(m => " " + m) .getOrElse("") +
      " import " + names.map(_.print).mkString(", ")
  }
}

case class Raise(exc: Option[iexpr], cause: Option[iexpr], lineno: Int, col_offset: Int) extends istmt {
  def this(exc: iexpr, cause: iexpr, attributeProvider: AttributeProvider) = {
    this(Option(exc), Option(cause), attributeProvider.lineno, attributeProvider.col_offset)
  }
  override def print: String = {
    "raise" + exc.map(e => " " + e.print).getOrElse("") + cause.map(c => " from " + c.print).getOrElse("")
  }
}

case class Pass(lineno: Int, col_offset: Int) extends istmt {
  def this(attributeProvider: AttributeProvider) = {
    this(attributeProvider.lineno, attributeProvider.col_offset)
  }
  override def print: String = {
    "pass"
  }
}

case class Del()

case class Assert(test: iexpr, msg: Option[iexpr], lineno: Int, col_offset: Int) extends istmt {
  def this(test: iexpr, msg: iexpr, attributeProvider: AttributeProvider) = {
    this(test, Option(msg), attributeProvider.lineno, attributeProvider.col_offset)
  }
  override def print: String = {
    "assert " + test.print + msg.map(m => ", " + m.print).getOrElse("")
  }
}

case class Break(lineno: Int, col_offset: Int) extends istmt {
  def this(attributeProvider: AttributeProvider) = {
    this(attributeProvider.lineno, attributeProvider.col_offset)
  }
  override def print: String = {
    "break"
  }
}

case class Continue(lineno: Int, col_offset: Int) extends istmt {
  def this(attributeProvider: AttributeProvider) = {
    this(attributeProvider.lineno, attributeProvider.col_offset)
  }
  override def print: String = {
    "continue"
  }
}

case class Global(names: Iterable[String], lineno: Int, col_offset: Int) extends istmt {
  def this(names: util.ArrayList[String], attributeProvider: AttributeProvider) = {
    this(names.asScala, attributeProvider.lineno, attributeProvider.col_offset)
  }
  override def print: String = {
    "global " + names.mkString(", ")
  }
}

case class Nonlocal(names: Iterable[String], lineno: Int, col_offset: Int) extends istmt {
  def this(names: util.ArrayList[String], attributeProvider: AttributeProvider) = {
    this(names.asScala, attributeProvider.lineno, attributeProvider.col_offset)
  }
  override def print: String = {
    "nonlocal " + names.mkString(", ")
  }
}

case class If(test: iexpr,
              body: Iterable[istmt],
              orelse: Iterable[istmt],
              lineno: Int,
              col_offset: Int) extends istmt {
  def this(test: iexpr,
           body: util.ArrayList[istmt],
           orelse: util.ArrayList[istmt],
           attributeProvider: AttributeProvider) = {
    this(test, body.asScala, orelse.asScala, attributeProvider.lineno, attributeProvider.col_offset)
  }
  override def print: String = {
    val elseString =
      orelse.size match {
        case 0 => ""
        case 1 if orelse.head.isInstanceOf[If] =>
          "\nel" + orelse.head.print
        case _ =>
          "\nelse:" +
            orelse.map(s => indent(s.print)).mkString("\n", "\n", "")
      }


    "if " + test.print + ":" +
      body.map(s => indent(s.print)).mkString("\n", "\n", "") +
      elseString
  }
}

case class While(test: iexpr,
                 body: Iterable[istmt],
                 orelse: Iterable[istmt],
                 lineno: Int,
                 col_offset: Int) extends istmt {
  def this(test: iexpr,
           body: util.ArrayList[istmt],
           orelse: util.ArrayList[istmt],
           attributeProvider: AttributeProvider) = {
    this(test, body.asScala, orelse.asScala, attributeProvider.lineno, attributeProvider.col_offset)
  }
  override def print: String = {
    "while " + test.print + ":" +
      body.map(s => indent(s.print)).mkString("\n", "\n", "") +
      (if (orelse.nonEmpty)
        "\nelse:" +
          orelse.map(s => indent(s.print)).mkString("\n", "\n", "")
      else ""
        )
  }
}

case class Try(body: Iterable[istmt],
               handlers: Iterable[iexcepthandler],
               orelse: Iterable[istmt],
               finalbody: Iterable[istmt],
               lineno: Int,
               col_offset: Int) extends istmt {
  def this(body: util.ArrayList[istmt],
           handlers: util.ArrayList[iexcepthandler],
           orelse: util.ArrayList[istmt],
           finalbody: util.ArrayList[istmt],
           attributeProvider: AttributeProvider) = {
    this(body.asScala,
      handlers.asScala,
      orelse.asScala,
      finalbody.asScala,
      attributeProvider.lineno,
      attributeProvider.col_offset)
  }
  override def print: String = {
    val elseString =
      if (orelse.nonEmpty) {
        "\nelse:" +
          orelse.map(s => indent(s.print)).mkString("\n", "\n", "")
      } else {
        ""
      }

    val finallyString =
      if (finalbody.nonEmpty) {
        "\nfinally:" +
          finalbody.map(s => indent(s.print)).mkString("\n", "\n", "")
      } else {
        ""
      }

    "try:" +
      body.map(s => indent(s.print)).mkString("\n", "\n", "") +
      handlers.map(_.print).mkString("\n", "\n", "") +
      elseString +
      finallyString
  }
}

case class ClassDef(name: String,
                    bases: Iterable[iexpr],
                    keywords: Iterable[ikeyword],
                    body: Iterable[istmt],
                    decorator_list: Iterable[iexpr],
                    lineno: Int,
                    col_offset: Int) extends istmt {
  def this(name: String,
           bases: util.ArrayList[iexpr],
           keywords: util.ArrayList[ikeyword],
           body: util.ArrayList[istmt],
           decorator_list: util.ArrayList[iexpr],
           attributeProvider: AttributeProvider) = {
    this(name,
      bases.asScala,
      keywords.asScala,
      body.asScala,
      decorator_list.asScala,
      attributeProvider.lineno,
      attributeProvider.col_offset)
  }
  override def print: String = {
    val optionArgEndComma = if (bases.nonEmpty && keywords.nonEmpty) ", " else ""

    decorator_list.map(d => "@" + d.print + "\n").mkString("") +
    "class " + name +
      "(" +
      bases.map(_.print).mkString(", ") +
      optionArgEndComma +
      keywords.map(_.print).mkString(", ") +
      ")" + ":" +
      body.map(s => indent(s.print)).mkString("\n", "\n", "")
  }
}
