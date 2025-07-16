package io.joern.swiftsrc2cpg.astcreation

import io.joern.swiftsrc2cpg.parser.SwiftNodeSyntax.*
import io.joern.x2cpg.frontendspecific.swiftsrc2cpg.Defines
import io.joern.x2cpg.{Ast, ValidationMode}
import io.shiftleft.codepropertygraph.generated.{ControlStructureTypes, EvaluationStrategies, PropertyNames}
import org.apache.commons.lang3.StringUtils

trait AstCreatorHelper(implicit withSchemaValidation: ValidationMode) { this: AstCreator =>

  protected def notHandledYet(node: SwiftNode): Ast = {
    val text =
      s"""Node type '${node.toString}' not handled yet!
         |  Code: '${code(node)}'
         |  File: '${parserResult.fullPath}'
         |  Line: ${line(node).getOrElse(-1)}
         |  Column: ${column(node).getOrElse(-1)}
         |  """.stripMargin
    logger.info(text)
    Ast(unknownNode(node, code(node)))
  }

  protected def astsForBlockElements(elements: List[SwiftNode]): List[Ast] = {
    val (deferElements: List[SwiftNode], otherElements: List[SwiftNode]) = elements.partition(n =>
      n.isInstanceOf[CodeBlockItemSyntax] && n.asInstanceOf[CodeBlockItemSyntax].item.isInstanceOf[DeferStmtSyntax]
    )
    val deferElementsAstsOrdered = deferElements.reverse.map(astForNode)
    val indexOfGuardStmt = otherElements.indexWhere(n =>
      n.isInstanceOf[CodeBlockItemSyntax] && n.asInstanceOf[CodeBlockItemSyntax].item.isInstanceOf[GuardStmtSyntax]
    )
    if (indexOfGuardStmt < 0) {
      otherElements.map(astForNode) ++ deferElementsAstsOrdered
    } else {
      val elementsBeforeGuard = otherElements.slice(0, indexOfGuardStmt)
      val guardStmt =
        otherElements(indexOfGuardStmt).asInstanceOf[CodeBlockItemSyntax].item.asInstanceOf[GuardStmtSyntax]
      val elementsAfterGuard = otherElements.slice(indexOfGuardStmt + 1, otherElements.size)

      val code         = this.code(guardStmt)
      val ifNode       = controlStructureNode(guardStmt, ControlStructureTypes.IF, code)
      val conditionAst = astForNode(guardStmt.conditions)

      val thenAst = astsForBlockElements(elementsAfterGuard) ++ deferElementsAstsOrdered match {
        case Nil => Ast()
        case blockElement :: Nil =>
          setOrderExplicitly(blockElement, 2)
          blockElement
        case blockChildren =>
          val block = blockNode(elementsAfterGuard.head).order(2)
          blockAst(block, blockChildren)
      }
      val elseAst = astForNode(guardStmt.body)
      setOrderExplicitly(elseAst, 3)

      val ifAst = controlStructureAst(ifNode, Option(conditionAst), Seq(thenAst, elseAst))
      astsForBlockElements(elementsBeforeGuard) :+ ifAst
    }
  }

  protected def astParentInfo(): (String, String) = {
    val astParentType     = methodAstParentStack.head.label
    val astParentFullName = methodAstParentStack.head.properties(PropertyNames.FullName).toString
    (astParentType, astParentFullName)
  }

  protected def astForIdentifier(node: SwiftNode): Ast = {
    val identifierName = code(node)
    val variableOption = scope.lookupVariable(identifierName)
    val tpe = variableOption match {
      case Some((_, variableTypeName)) => variableTypeName
      case _                           => Defines.Any
    }
    val identNode = identifierNode(node, identifierName).typeFullName(tpe)
    scope.addVariableReference(identifierName, identNode, tpe, EvaluationStrategies.BY_REFERENCE)
    Ast(identNode)
  }

  protected def cleanName(name: String): String = {
    if (name == Defines.Any) return name
    val normalizedName = StringUtils.normalizeSpace(name)
    stripTemplateTags(normalizedName)
  }

  private val TagsToKeepInFullName =
    List("<anonymous>", "<lambda>", "<global>", "<type>", "<extension>", "<wildcard>")

  /** Removes template type parameters from qualified names while preserving special tags.
    *
    * This method strips the angle brackets and their contents from type names, which is useful for simplifying complex
    * templated type names. It preserves certain special tags that are enclosed in angle brackets (like &lt;lambda&gt;,
    * &lt;const&gt;, etc.) to maintain semantic meaning.
    *
    * Examples:
    * {{{
    *  stripTemplateTags("ns.Foo<int>") == "ns.Foo"
    *  stripTemplateTags("Foo.Bar<T>.<lambda>1") == "Foo.Bar.<lambda>1" // preserves the special <lambda> tag
    *  stripTemplateTags("ns.map<ns.foo, ns.bar<int>>") == "ns.map" // removes nested template parameters
    * }}}
    *
    * @param input
    *   The input string that may contain template tags
    * @return
    *   The string with template tags removed but special tags preserved
    */
  private def stripTemplateTags(input: String): String = {
    if (input.isEmpty || !input.contains("<") || !input.contains(">")) {
      return input
    }

    val firstOpenIndex = input.indexOf("<")
    // Find matching closing bracket, accounting for nesting
    var nesting    = 1
    var closeIndex = firstOpenIndex + 1
    while (closeIndex < input.length && nesting > 0) {
      if (input(closeIndex) == '<') nesting += 1
      else if (input(closeIndex) == '>') nesting -= 1
      closeIndex += 1
    }
    closeIndex -= 1 // Adjust to point at the closing bracket

    val prefix = input.substring(0, firstOpenIndex)
    val tag    = input.substring(firstOpenIndex, closeIndex + 1)
    val suffix = input.substring(closeIndex + 1)

    // Keep special tags, remove others
    if (TagsToKeepInFullName.contains(tag)) {
      s"$prefix$tag${stripTemplateTags(suffix)}"
    } else {
      s"$prefix${stripTemplateTags(suffix)}"
    }
  }

  protected def cleanType(rawType: String): String = {
    if (rawType == Defines.Any) return rawType
    val normalizedTpe = StringUtils.normalizeSpace(rawType.stripSuffix(" ()"))
    stripTemplateTags(normalizedTpe) match {
      // Empty or problematic types
      case ""                   => Defines.Any
      case t if t.contains("?") => Defines.Any
      // Special patterns with specific handling
      case t if t.startsWith("[") && t.endsWith("]") => Defines.Array
      case t if t.contains("=>") || t.contains("->") => Defines.Function
      case t if t.contains("( ")                     => t.substring(0, t.indexOf("( "))
      // Default case
      case typeStr => typeStr
    }
  }

  protected def registerType(typeFullName: String): Unit = {
    global.usedTypes.putIfAbsent(typeFullName, true)
  }

  protected def scopeLocalUniqueName(targetName: String): String = {
    val name = if (targetName.nonEmpty) { s"<$targetName>" }
    else { "<anonymous>" }
    val key = s"${scope.computeScopePath}:$name"
    val idx = scopeLocalUniqueNames.getOrElseUpdate(key, 0)
    scopeLocalUniqueNames.update(key, idx + 1)
    s"$name$idx"
  }

  protected def calcNameAndFullName(name: String): (String, String) = {
    val fullNamePrefix = s"${parserResult.filename}:${scope.computeScopePath.replaceAll(":", ".")}."
    val fullName       = s"$fullNamePrefix$name"
    (name, fullName)
  }

  protected def calcMethodName(func: SwiftNode): String = {
    val name = func match {
      case f: FunctionDeclSyntax      => code(f.name)
      case a: AccessorDeclSyntax      => code(a.accessorSpecifier)
      case d: DeinitializerDeclSyntax => code(d.deinitKeyword)
      case i: InitializerDeclSyntax   => code(i.initKeyword)
      case s: SubscriptDeclSyntax     => code(s.subscriptKeyword)
      case _                          => nextClosureName()
    }
    cleanName(name)
  }

}
