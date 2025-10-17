package io.joern.swiftsrc2cpg.astcreation

import io.joern.swiftsrc2cpg.parser.SwiftNodeSyntax.*
import io.joern.x2cpg.frontendspecific.swiftsrc2cpg.Defines
import io.joern.x2cpg.{Ast, ValidationMode}
import io.shiftleft.codepropertygraph.generated.{ControlStructureTypes, EvaluationStrategies, PropertyNames}
import org.apache.commons.lang3.StringUtils

object AstCreatorHelper {

  private val TagsToKeepInFullName = List("<anonymous>", "<lambda>", "<global>", "<type>", "<extension>", "<wildcard>")

  /** Removes generic type parameters from qualified names while preserving special tags.
    *
    * This method strips the angle brackets and their contents from type names, which is useful for simplifying complex
    * generic type and declaration full names. It preserves certain special tags that are enclosed in angle brackets
    * (like &lt;lambda&gt;, &lt;const&gt;, etc.) to maintain semantic meaning.
    *
    * Examples:
    * {{{
    *  stripGenerics("ns.Foo<int>") == "ns.Foo"
    *  stripGenerics("Foo.Bar<T>.<lambda>1") == "Foo.Bar.<lambda>1" // preserves the special <lambda> tag
    *  stripGenerics("ns.map<ns.foo, ns.bar<int>>") == "ns.map" // removes nested generic parameters
    * }}}
    *
    * @param input
    *   The input string that may contain generic types
    * @return
    *   The string with generic types removed but special tags preserved
    */
  def stripGenerics(input: String): String = {
    if (input.isEmpty || !input.contains("<") || !input.contains(">")) {
      return input
    }

    val firstOpenIndex = input.indexOf("<")
    // Find matching closing bracket, accounting for nesting
    var nesting    = 1
    var closeIndex = firstOpenIndex + 1
    while (closeIndex < input.length && nesting > 0) {
      if (input(closeIndex) == '<') nesting += 1
      else if (input(closeIndex) == '>' && closeIndex > 0 && input(closeIndex - 1) != '-') nesting -= 1
      closeIndex += 1
    }
    closeIndex -= 1 // Adjust to point at the closing bracket

    val prefix = input.substring(0, firstOpenIndex)
    val tag    = input.substring(firstOpenIndex, closeIndex + 1)
    val suffix = input.substring(closeIndex + 1)

    // Keep special tags, remove others
    if (isSpecialTag(tag, suffix)) {
      s"$prefix$tag${stripGenerics(suffix)}"
    } else {
      s"$prefix${stripGenerics(suffix)}"
    }
  }

  private def isSpecialTag(tag: String, suffix: String): Boolean = {
    TagsToKeepInFullName.contains(tag) || suffix.startsWith(" infix") || suffix.startsWith(" prefix")
  }

  def cleanName(name: String): String = {
    if (name == Defines.Any) return name
    val normalizedName = StringUtils.normalizeSpace(name)
    stripGenerics(normalizedName)
  }

  def cleanType(rawType: String): String = {
    if (rawType == Defines.Any) return rawType
    val normalizedTpe = StringUtils.normalizeSpace(rawType.stripSuffix(" ()"))
    stripGenerics(normalizedTpe) match {
      // Empty or problematic types
      case ""                   => Defines.Any
      case t if t.contains("?") => Defines.Any
      // Map builtin types
      case "String"     => Defines.String
      case "Character"  => Defines.Character
      case "Int"        => Defines.Int
      case "Float"      => Defines.Float
      case "Double"     => Defines.Double
      case "Bool"       => Defines.Bool
      case "Array"      => Defines.Array
      case "Dictionary" => Defines.Dictionary
      case "Nil"        => Defines.Nil
      // Special patterns with specific handling
      case t if t.startsWith("[") && t.endsWith("]")  => Defines.Array
      case t if t.startsWith("(") && t.contains("->") => Defines.Function
      case t if t.contains("( ")                      => t.substring(0, t.indexOf("( "))
      // Default case
      case typeStr => typeStr
    }
  }

}

trait AstCreatorHelper(implicit withSchemaValidation: ValidationMode) { this: AstCreator =>

  import AstCreatorHelper.*

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
    val identNode      = identifierNode(node, identifierName)
    val variableOption = scope.lookupVariable(identifierName)
    val tpe = variableOption match {
      case Some((_, variableTypeName)) if variableTypeName != Defines.Any => variableTypeName
      case None if identNode.typeFullName != Defines.Any                  => identNode.typeFullName
      case _                                                              => Defines.Any
    }
    identNode.typeFullName = tpe
    scope.addVariableReference(identifierName, identNode, tpe, EvaluationStrategies.BY_REFERENCE)
    Ast(identNode)
  }

  protected def registerType(typeFullName: String): Unit = {
    global.usedTypes.putIfAbsent(typeFullName, true)
  }

  protected def scopeLocalUniqueName(targetName: String): String = {
    val name = if (targetName.nonEmpty) { s"<$targetName>" }
    else { "<anonymous>" }
    val key = s"${scope.computeScopePath}.$name"
    val idx = scopeLocalUniqueNames.getOrElseUpdate(key, 0)
    scopeLocalUniqueNames.update(key, idx + 1)
    s"$name$idx"
  }

  protected def methodInfoFromFullNameWithSignature(fullNameWithSignature: String): (String, String) = {
    if (fullNameWithSignature.contains("(")) {
      (
        fullNameWithSignature.substring(0, fullNameWithSignature.indexOf("(")),
        fullNameWithSignature.substring(fullNameWithSignature.indexOf("("))
      )
    } else {
      (fullNameWithSignature, "()")
    }
  }

  private val ReturnTypeMatcher = """\(.*\)(->|:)(.+)""".r

  protected def methodInfoForFunctionDeclLike(node: FunctionDeclLike): MethodInfo = {
    val name = calcMethodName(node)
    fullnameProvider.declFullname(node) match {
      case Some(fullNameWithSignature) =>
        val (fullName, signature) = methodInfoFromFullNameWithSignature(fullNameWithSignature)
        val returnType = node match {
          case _: DeinitializerDeclSyntax =>
            Defines.Void
          case _: InitializerDeclSyntax =>
            ReturnTypeMatcher.findFirstMatchIn(signature).map(_.group(2)).getOrElse(astParentInfo()._2)
          case _ =>
            ReturnTypeMatcher.findFirstMatchIn(signature).map(_.group(2)).getOrElse(Defines.Any)
        }
        registerType(returnType)
        MethodInfo(name, fullName, signature, returnType)
      case None =>
        val (methodName, methodFullName) = calcNameAndFullName(name)
        val (signature, returnType) = node match {
          case f: FunctionDeclSyntax =>
            val returnType = f.signature.returnClause.fold(Defines.Any)(c => cleanType(code(c.`type`)))
            (s"${paramSignature(f.signature.parameterClause)}->$returnType", returnType)
          case a: AccessorDeclSyntax =>
            (Defines.Any, Defines.Any)
          case i: InitializerDeclSyntax =>
            val (_, returnType) = astParentInfo()
            (s"${paramSignature(i.signature.parameterClause)}->$returnType", returnType)
          case _: DeinitializerDeclSyntax =>
            val returnType = Defines.Any
            (s"()->$returnType", returnType)
          case s: SubscriptDeclSyntax =>
            val returnType = cleanType(code(s.returnClause.`type`))
            (s"${paramSignature(s.parameterClause)}->$returnType", returnType)
          case c: ClosureExprSyntax =>
            fullnameProvider.typeFullnameRaw(node) match {
              case Some(tpe) =>
                val signature  = tpe
                val returnType = ReturnTypeMatcher.findFirstMatchIn(signature).map(_.group(2)).getOrElse(Defines.Any)
                (signature, returnType)
              case _ =>
                val returnType = c.signature.flatMap(_.returnClause).fold(Defines.Any)(r => cleanType(code(r.`type`)))
                val paramClauseCode = c.signature.flatMap(_.parameterClause).fold("()")(paramSignature)
                (s"$paramClauseCode->$returnType", returnType)
            }
        }
        registerType(returnType)
        MethodInfo(methodName, methodFullName, signature, returnType)
    }
  }

  case class MethodInfo(name: String, fullName: String, signature: String, returnType: String) {
    val fullNameAndSignature: String = s"$fullName:$signature"
  }

  case class TypeInfo(name: String, fullName: String)

  protected def methodInfoForAccessorDecl(node: AccessorDeclSyntax, variableName: String, tpe: String): MethodInfo = {
    val accessorName = code(node.accessorSpecifier)
    val name = accessorName match {
      case "set" => s"$variableName.setter"
      case "get" => s"$variableName.getter"
      case _     => s"$variableName.$accessorName"
    }

    fullnameProvider.declFullname(node) match {
      case Some(fullNameWithSignature) =>
        var fullName  = fullNameWithSignature
        var signature = tpe
        if (fullNameWithSignature.contains(":")) {
          fullName = fullNameWithSignature.substring(0, fullNameWithSignature.lastIndexOf(":"))
          signature = fullNameWithSignature.substring(fullNameWithSignature.lastIndexOf(":"))
        }
        MethodInfo(name, fullName, signature, tpe)
      case None =>
        val (methodName, methodFullName) = calcNameAndFullName(name)
        registerType(tpe)
        MethodInfo(methodName, methodFullName, tpe, tpe)
    }
  }

  protected def typeNameInfoForDeclSyntax(node: DeclSyntax): TypeInfo = {
    val name = typeNameForDeclSyntax(node)
    typeNameInfoForNode(node, name)
  }

  protected def nameForTypeSyntax(node: TypeSyntax): String = {
    val name = node match {
      case _: ArrayTypeSyntax                       => "array-type"
      case a: AttributedTypeSyntax                  => nameForTypeSyntax(a.baseType)
      case _: ClassRestrictionTypeSyntax            => "class-restriction-type"
      case _: CompositionTypeSyntax                 => "composition-type"
      case _: DictionaryTypeSyntax                  => "dictionary-type"
      case _: FunctionTypeSyntax                    => Defines.Function
      case id: IdentifierTypeSyntax                 => code(id.name)
      case w: ImplicitlyUnwrappedOptionalTypeSyntax => nameForTypeSyntax(w.wrappedType)
      case m: MemberTypeSyntax                      => code(m)
      case m: MetatypeTypeSyntax                    => code(m)
      case _: MissingTypeSyntax                     => "missing-type"
      case n: NamedOpaqueReturnTypeSyntax           => nameForTypeSyntax(n.`type`)
      case o: OptionalTypeSyntax                    => nameForTypeSyntax(o.wrappedType)
      case p: PackElementTypeSyntax                 => nameForTypeSyntax(p.pack)
      case p: PackExpansionTypeSyntax               => nameForTypeSyntax(p.repetitionPattern)
      case s: SomeOrAnyTypeSyntax                   => nameForTypeSyntax(s.constraint)
      case s: SuppressedTypeSyntax                  => nameForTypeSyntax(s.`type`)
      case _: TupleTypeSyntax                       => "tuple-type"
    }
    scopeLocalUniqueName(cleanType(name))
  }

  protected def typeNameInfoForTypeSyntax(node: TypeSyntax): TypeInfo = {
    val name = nameForTypeSyntax(node)
    typeNameInfoForNode(node, name)
  }

  private def typeNameInfoForNode(node: SwiftNode, name: String): TypeInfo = {
    fullnameProvider.declFullname(node) match {
      case Some(declFullname) =>
        registerType(declFullname)
        TypeInfo(name, declFullname)
      case None =>
        val (_, declFullname) = calcNameAndFullName(name)
        registerType(declFullname)
        TypeInfo(name, declFullname)
    }
  }

  private def calcNameAndFullName(name: String): (String, String) = {
    val fullNamePrefix = s"${parserResult.filename}:${scope.computeScopePath}"
    val fullName       = s"$fullNamePrefix.$name"
    (name, fullName)
  }

  private def calcMethodName(func: SwiftNode): String = {
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
