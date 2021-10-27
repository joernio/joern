package io.shiftleft.console

import io.shiftleft.codepropertygraph.generated.nodes
import pprint.{PPrinter, Renderer, Result, Tree, Truncated}

object pprinter {

  val AnsiEncodedRegexp = "\u001b\\[[\\d;]+m".r
  def isAnsiEncoded(s: String): Boolean =
    AnsiEncodedRegexp.findFirstIn(s).isDefined

  /** We use source-highlight to encode source as ansi strings, e.g. the .dump step
    * Ammonite uses fansi for it's colour-coding, and while both pledge to follow the ansi codec, they aren't compatible
    * TODO: PR for fansi to support these standard encodings out of the box
    * */
  def fixForFansi(ansiEncoded: String): String =
    ansiEncoded
      .replaceAll("\u001b\\[m", "\u001b[39m") //encoding ends with [39m for fansi instead of [m
      .replaceAll("\u001b\\[0(\\d)m", "\u001b[$1m") // `[01m` is encoded as `[1m` in fansi for all single digit numbers
      .replaceAll("\u001b\\[0?(\\d+);0?(\\d+)m", "\u001b[$1m\u001b[$2m") // `[01;34m` is encoded as `[1m[34m` in fansi
      .replaceAll("\u001b\\[[00]+;0?(\\d+);0?(\\d+);0?(\\d+)m", "\u001b[$1;$2;$3m") // `[00;38;05;70m` is encoded as `[38;5;70m` in fansi - 8bit color encoding

  def create(original: PPrinter): PPrinter =
    new PPrinter(defaultHeight = 99999, additionalHandlers = myAdditionalHandlers(original)) {
      override def tokenize(x: Any,
                            width: Int = defaultWidth,
                            height: Int = defaultHeight,
                            indent: Int = defaultIndent,
                            initialOffset: Int = 0): Iterator[fansi.Str] = {
        val tree = this.treeify(x)
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

  private def myAdditionalHandlers(original: PPrinter): PartialFunction[Any, Tree] = {
    case node: nodes.StoredNode =>
      Tree.Apply(
        node.productPrefix,
        Iterator.range(0, node.productArity).map { n =>
          Tree.Infix(Tree.Literal(node.productElementLabel(n)), "->", original.treeify(node.productElement(n)))
        }
      )
  }
}
