package io.joern.console

import pprint.{PPrinter, Renderer, Result, Tree, Truncated}

import scala.util.Try

object PPrinter {
  private val printer = pprinter.create(pprint.PPrinter.BlackWhite)

  def apply(obj: Object, maxElements: Int): String = {
    printer.apply(obj).toString
  }
}

object pprinter {

  val AnsiEncodedRegexp = "\u001b\\[[\\d;]+m".r
  def isAnsiEncoded(s: String): Boolean =
    AnsiEncodedRegexp.findFirstIn(s).isDefined

  /** We use source-highlight to encode source as ansi strings, e.g. the .dump step Ammonite uses fansi for it's
    * colour-coding, and while both pledge to follow the ansi codec, they aren't compatible TODO: PR for fansi to
    * support these standard encodings out of the box
    */
  def fixForFansi(ansiEncoded: String): String =
    ansiEncoded
      .replaceAll("\u001b\\[m", "\u001b[39m")       // encoding ends with [39m for fansi instead of [m
      .replaceAll("\u001b\\[0(\\d)m", "\u001b[$1m") // `[01m` is encoded as `[1m` in fansi for all single digit numbers
      .replaceAll("\u001b\\[0?(\\d+);0?(\\d+)m", "\u001b[$1m\u001b[$2m") // `[01;34m` is encoded as `[1m[34m` in fansi
      .replaceAll(
        "\u001b\\[[00]+;0?(\\d+);0?(\\d+);0?(\\d+)m",
        "\u001b[$1;$2;$3m"
      ) // `[00;38;05;70m` is encoded as `[38;5;70m` in fansi - 8bit color encoding

  def create(original: PPrinter): PPrinter =
    new PPrinter(
      defaultHeight = 99999,
      colorLiteral = fansi.Attrs.Empty, // leave color highlighting to the repl
      colorApplyPrefix = fansi.Attrs.Empty,
      additionalHandlers = handleProduct(original)) {
      override def tokenize(
        x: Any,
        width: Int = defaultWidth,
        height: Int = defaultHeight,
        indent: Int = defaultIndent,
        initialOffset: Int = 0,
        escapeUnicode: Boolean,
        showFieldNames: Boolean
      ): Iterator[fansi.Str] = {
        val tree = this.treeify(x, escapeUnicode = escapeUnicode, showFieldNames = showFieldNames)
        val renderer = new Renderer(width, colorApplyPrefix, colorLiteral, indent) {
          override def rec(x: Tree, leftOffset: Int, indentCount: Int): Result = x match {
            case Tree.Literal(body) if isAnsiEncoded(body) =>
              // this is the part we're overriding, everything else is just boilerplate
              Result.fromString(fixForFansi(body))
            case _ => super.rec(x, leftOffset, indentCount)
          }
        }
        val rendered = renderer.rec(tree, initialOffset, 0).iter
        new Truncated(rendered, width, height)
      }
    }

  private def handleProduct(original: PPrinter): PartialFunction[Any, Tree] = {
    case product: Product =>
      Tree.Apply(
        product.productPrefix,
        Iterator.range(0, product.productArity).map { elementIdx =>
          val rightSide = original.treeify(
              product.productElement(elementIdx),
              escapeUnicode = original.defaultEscapeUnicode,
              showFieldNames = original.defaultShowFieldNames
            )
          productElementNameMaybe(product, elementIdx) match {
            case Some(name) => Tree.Infix(Tree.Literal(name), "->", rightSide)
            case None => rightSide
          }
        }
      )
  }

  private def productElementNameMaybe(product: Product, elementIdx: Int): Option[String] =
    try {
      // this may fail, e.g. if product doesn't override `productElementName`...
      val a = product.productElementName(elementIdx)
      println(s"AAAA0 a=$a;")
      Some("foooooo")
//      Some(a)
    } catch {
      case t: Throwable =>
        println("AAAA1")
        t.printStackTrace()
        None
    }

//  private def productElementNameMaybe(product: Product, elementIdx: Int): Option[String] =
//    Try {
//      // this may fail, e.g. if product doesn't override `productElementName`...
////      product.productElementName(elementIdx)
//      "foooooo"
//    }.toOption
}
