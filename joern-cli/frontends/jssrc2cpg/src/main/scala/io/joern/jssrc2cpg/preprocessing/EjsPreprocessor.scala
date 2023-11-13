package io.joern.jssrc2cpg.preprocessing

import scala.collection.mutable

class EjsPreprocessor {

  private val CommentTag        = "<%#"
  private val TagGroupsRegex    = """(<%[=\-_#]?)([\s\S]*?)([-_#]?%>)""".r
  private val ScriptGroupsRegex = """(<script>)([\s\S]*?)(</script>)""".r
  private val OpeningTags       = List("<%#", "<%=", "<%-", "<%_")
  private val ClosingTags       = List("-%>", "_%>", "#%>", "%>")
  private val Tags              = OpeningTags ++ ClosingTags

  private def stripScriptTag(code: String): String = {
    var x = code.replaceAll("<script>", "<%      ").replaceAll("</script>", "%>       ")
    ScriptGroupsRegex.findAllIn(code).matchData.foreach { ma =>
      var scriptBlock = ma.group(2)
      val matches     = TagGroupsRegex.findAllIn(scriptBlock).matchData.toList
      matches.foreach {
        case mat if mat.group(1) == "<%" && mat.group(3) == "-%>" =>
          scriptBlock = scriptBlock.replace(mat.toString(), " " * mat.toString().replaceAll("[^\\s]", " ").length)
        case _ =>
      }
      OpeningTags.foreach { tag =>
        scriptBlock = scriptBlock.replaceAll(s"'$tag", s"\"${" " * (tag.length - 1)}")
      }
      ClosingTags.foreach { tag =>
        scriptBlock = scriptBlock.replaceAll(s"$tag'", s"${" " * (tag.length - 1)}\"")
      }
      Tags.foreach { tag =>
        scriptBlock = scriptBlock.replaceAll(tag, " " * tag.length)
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
    }

    codeAsCharArray.zipWithIndex.foreach {
      case (currChar, _) if currChar == '\n' || currChar == '\r' =>
        preprocessedCode.append(currChar)
      case (currChar, index) if positions.exists { case (start, end) => index >= start && index < end } =>
        preprocessedCode.append(currChar)
      case _ =>
        preprocessedCode.append(" ")
    }

    var codeWithoutSemicolon = preprocessedCode.toString()
    val alreadyReplaced      = mutable.ArrayBuffer.empty[(Int, Int)]
    matches.foreach {
      case ma if ma.group(1) == CommentTag               => // ignore comments
      case ma if ma.group(2).trim.startsWith("include ") => // ignore including other ejs templates
      case ma if needsSemicolon(ma.group(2)) =>
        val start = ma.start + ma.group(1).length
        val end   = ma.end - ma.group(3).length
        if (!alreadyReplaced.contains((start, end))) {
          val replacementCode = s"${ma.group(2)};"
          codeWithoutSemicolon =
            s"${codeWithoutSemicolon.substring(0, start)}$replacementCode${codeWithoutSemicolon.substring(end + 1, codeWithoutSemicolon.length)}"
          alreadyReplaced.append((start, end))
        }
      case _ => // others are fine already
    }

    codeWithoutSemicolon
  }

}
