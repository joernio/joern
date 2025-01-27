package io.joern.c2cpg.astcreation

import org.eclipse.cdt.core.dom.ast.{
  IASTFileLocation,
  IASTName,
  IASTPreprocessorElifStatement,
  IASTPreprocessorIfStatement,
  IASTPreprocessorMacroExpansion,
  IASTTranslationUnit
}
import org.eclipse.cdt.core.parser.IToken
import org.eclipse.cdt.core.parser.util.CharArrayMap
import org.eclipse.cdt.internal.core.parser.scanner.Lexer.LexerOptions
import org.eclipse.cdt.internal.core.parser.scanner.*

import scala.annotation.nowarn
import scala.collection.mutable

/** Utility class that obtains the arguments of the macro at `loc` of the translation unit `tu` (we used CDT's naming
  * convention here.) The way this works is by performing macro expansion using CDT's `MacroExpander`, which accepts a
  * `MacroExpansionTracker`, which is informed of arguments as they are determined. The problem is that the default
  * `MacroExpansionTracker` does not make these arguments available separately but only in a form where all arguments
  * have been merged back into a string.
  *
  * By supplying a custom tracker that inherits from `MacroExpansionTracker` and overrides the method
  * `setExpandedMacroArgument`, we can intercept arguments and store them in a list for later retrieval. We wrap this
  * rather complicated way of accessing the macro arguments in the single public method `getArguments` of the
  * `MacroArgumentExtractor`.
  */
class MacroArgumentExtractor(tu: IASTTranslationUnit, loc: IASTFileLocation) {

  def getArguments: List[String] = {
    val resolver  = tu.getAdapter(classOf[ILocationResolver])
    val expansion = resolver.getMacroExpansions(loc).headOption
    expansion.toList.flatMap { exp =>
      val dictionary: CharArrayMap[PreprocessorMacro] = createDictionary(exp)
      val lexerOptions                                = tu.getAdapter(classOf[LexerOptions])
      val expander       = new MacroExpander(ILexerLog.NULL, dictionary, null, lexerOptions)
      val tracker        = new C2CpgMacroExpansionTracker(Integer.MAX_VALUE)
      val source: String = resolver.getUnpreprocessedSignature(loc).mkString("")
      val refLoc         = exp.getFileLocation
      val input          = source.slice(0, refLoc.getNodeLength)
      val enclosing      = tu.getNodeSelector(null).findEnclosingNode(-1, 2)
      val isPPCondition = enclosing.isInstanceOf[IASTPreprocessorIfStatement] || enclosing
        .isInstanceOf[IASTPreprocessorElifStatement]
      expander.expand(input, tracker, tu.getFilePath, refLoc.getStartingLineNumber, isPPCondition)
      tracker.arguments.toList
    }
  }

  private def createDictionary(expansion: IASTPreprocessorMacroExpansion): CharArrayMap[PreprocessorMacro] = {
    val refs: Array[IASTName] = Array(expansion.getMacroReference)
    val map                   = new CharArrayMap[PreprocessorMacro](refs.length)
    refs.foreach(name => addMacroDefinition(map, name))
    map
  }

  private def addMacroDefinition(map: CharArrayMap[PreprocessorMacro], name: IASTName): Unit = {
    val binding = name.getBinding
    binding match {
      case preprocessorMacro: PreprocessorMacro =>
        map.put(name.getSimpleID, preprocessorMacro);
      case _ =>
    }
  }
}

class C2CpgMacroExpansionTracker(stepToTrack: Int) extends MacroExpansionTracker(stepToTrack) {
  val arguments: mutable.Buffer[String] = mutable.Buffer()

  @nowarn
  override def setExpandedMacroArgument(tokenList: TokenList): Unit = {
    if (tokenList != null) {
      arguments.addOne(tokenListToString(tokenList))
    } else {
      arguments.addOne("")
    }
  }

  private def tokenListToString(tokenList: TokenList): String = {
    var tok: IToken = tokenList.first()
    var arg         = ""
    while (tok != null) {
      arg += s"${tok.toString} "
      tok = tok.getNext
    }
    arg.trim
  }

}
