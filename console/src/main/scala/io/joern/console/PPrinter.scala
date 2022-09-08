package io.joern.console

import pprint.{Renderer, Result, Tree, Truncated}

object PPrinter {
  private val pprinter = create()

  def apply(obj: Object): String =
    pprinter(obj).toString

  private def create(): pprint.PPrinter = {
    new pprint.PPrinter(
      defaultHeight = 99999,
      colorLiteral = fansi.Attrs.Empty, // leave color highlighting to the repl
      colorApplyPrefix = fansi.Attrs.Empty,
      additionalHandlers = handleProduct(pprint.PPrinter.BlackWhite)) {

      override def tokenize(x: Any,
                            width: Int = defaultWidth,
                            height: Int = defaultHeight,
                            indent: Int = defaultIndent,
                            initialOffset: Int = 0,
                            escapeUnicode: Boolean,
                            showFieldNames: Boolean): Iterator[fansi.Str] = {
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
  }

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

  private def handleProduct(original: pprint.PPrinter): PartialFunction[Any, Tree] = {
    case product: Product =>
      Tree.Apply(
        product.productPrefix,
        Iterator.range(0, product.productArity).map { elementIdx =>
          val elementValueTree = original.treeify(
            product.productElement(elementIdx),
            escapeUnicode = original.defaultEscapeUnicode,
            showFieldNames = original.defaultShowFieldNames
          )
          product.productElementName(elementIdx) match {
            case "" => elementValueTree
            case name => Tree.Infix(Tree.Literal(name), "->", elementValueTree)
          }
        }
      )
  }

}
