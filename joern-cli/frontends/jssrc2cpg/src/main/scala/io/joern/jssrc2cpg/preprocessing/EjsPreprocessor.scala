package io.joern.jssrc2cpg.preprocessing

import scala.collection.mutable

class EjsPreprocessor {

  private val CommentTag             = "<%#"
  private val TagGroupsRegex         = """(<%[=\-_#]?)([\s\S]*?)([-_#]?%>)""".r
  private val ScriptGroupsRegex      = """(<script>)([\s\S]*?)(</script>)""".r
  private val OpeningTags            = List("<%#", "<%=", "<%-", "<%_")
  private val ClosingTags            = List("-%>", "_%>", "#%>", "%>")
  private val Tags                   = OpeningTags ++ ClosingTags
  private val TagSpaces              = Tags.map(tag => tag -> (" " * tag.length)).toMap
  private val OpeningTagReplacements = OpeningTags.map(tag => ("'" + tag) -> ("\"" + " " * (tag.length - 1)))
  private val ClosingTagReplacements = ClosingTags.map(tag => (tag + "'") -> (" " * (tag.length - 1) + "\""))
  private val OutputTags             = Set("<%=", "<%-")
  private val FakeOutputCall         = "ap" // valid 2-char JS identifier; makes the parser emit a CallExpression

  private def stripScriptTag(code: String): String = {
    var x = code.replace("<script>", "<%      ").replace("</script>", "%>       ")
    ScriptGroupsRegex.findAllIn(code).matchData.foreach { ma =>
      var scriptBlock = ma.group(2)
      val matches     = TagGroupsRegex.findAllIn(scriptBlock).matchData.toList
      matches.foreach {
        case mat if mat.group(1) == "<%" && mat.group(3) == "-%>" =>
          scriptBlock = scriptBlock.replace(mat.toString(), " " * mat.toString().length)
        case _ =>
      }
      OpeningTagReplacements.foreach { case (search, replacement) =>
        scriptBlock = scriptBlock.replace(search, replacement)
      }
      ClosingTagReplacements.foreach { case (search, replacement) =>
        scriptBlock = scriptBlock.replace(search, replacement)
      }
      TagSpaces.foreach { case (tag, spaces) =>
        scriptBlock = scriptBlock.replace(tag, spaces)
      }
      x = x.replace(ma.group(2), scriptBlock)
    }
    x
  }

  private def needsSemicolon(code: String): Boolean =
    !code.trim.endsWith("{") && !code.trim.endsWith("}") && !code.trim.endsWith(";")

  def preprocess(code: String): String = {
    val codeWithoutScriptTag = stripScriptTag(code)
    val codeAsCharArray      = codeWithoutScriptTag.toCharArray
    val preprocessedCode     = new mutable.StringBuilder(codeAsCharArray.length)
    val matches              = TagGroupsRegex.findAllIn(codeWithoutScriptTag).matchData.toList

    val positions = matches.flatMap {
      case ma if ma.group(1) == CommentTag               => None // ignore comments
      case ma if ma.group(2).trim.startsWith("include ") => None // ignore including other ejs templates
      case ma =>
        val start = ma.start + ma.group(1).length
        val end   = ma.end - ma.group(3).length
        Option((start, end))
    }.toArray

    // Keep characters inside a tag body (or newlines); blank everything else out. `positions` are sorted and
    // non-overlapping, so a single advancing pointer replaces the previous per-character linear scan.
    var positionIndex = 0
    var index         = 0
    while (index < codeAsCharArray.length) {
      val currChar = codeAsCharArray(index)
      while (positionIndex < positions.length && index >= positions(positionIndex)._2) positionIndex += 1
      val insideTagBody = positionIndex < positions.length && index >= positions(positionIndex)._1
      if (currChar == '\n' || currChar == '\r' || insideTagBody) preprocessedCode.append(currChar)
      else preprocessedCode.append(' ')
      index += 1
    }

    matches.foreach {
      case ma if ma.group(1) == CommentTag               => // ignore comments
      case ma if ma.group(2).trim.startsWith("include ") => // ignore including other ejs templates
      case ma if OutputTags.contains(ma.group(1))        =>
        // opening 3-char tag (<%= / <%-) -> `ap(`
        preprocessedCode.setCharAt(ma.start, FakeOutputCall.charAt(0))
        preprocessedCode.setCharAt(ma.start + 1, FakeOutputCall.charAt(1))
        preprocessedCode.setCharAt(ma.start + 2, '(')
        // close the call: `);` at the start of the closing tag; any extra closing chars stay whitespace
        val closeStart = ma.end - ma.group(3).length
        preprocessedCode.setCharAt(closeStart, ')')
        preprocessedCode.setCharAt(closeStart + 1, ';')
      case ma if needsSemicolon(ma.group(2)) =>
        // scriptlet needing a statement terminator: overwrite the first closing-tag char with `;`
        val closeStart = ma.end - ma.group(3).length
        preprocessedCode.setCharAt(closeStart, ';')
      case _ => // others are fine already
    }

    preprocessedCode.toString()
  }

}
