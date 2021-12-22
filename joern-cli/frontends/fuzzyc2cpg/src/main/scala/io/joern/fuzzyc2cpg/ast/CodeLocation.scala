package io.joern.fuzzyc2cpg.ast

object CodeLocation {
  def apply: CodeLocation = new CodeLocation()
}

case class CodeLocation(
    startLine: Option[Int] = None,
    startPos: Option[Int] = None,
    startIndex: Option[Int] = None,
    endIndex: Option[Int] = None,
    endLine: Option[Int] = None,
    endPos: Option[Int] = None
) {

  override def toString: String =
    String.format(
      "%d:%d:%d:%d:%d:%d",
      startLine.getOrElse(-1),
      startPos.getOrElse(-1),
      startIndex.getOrElse(-1),
      endIndex.getOrElse(-1),
      endLine.getOrElse(-1),
      endPos.getOrElse(-1)
    )
}
