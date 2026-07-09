package io.joern.swiftsrc2cpg.astcreation

import io.joern.swiftsrc2cpg.parser.SwiftNodeSyntax.*
import io.joern.x2cpg.frontendspecific.swiftsrc2cpg.Defines
import io.joern.x2cpg.{Ast, ValidationMode}
import io.joern.x2cpg.datastructures.Stack.*
import io.joern.x2cpg.datastructures.VariableScopeManager
import io.shiftleft.codepropertygraph.generated.*
import io.shiftleft.codepropertygraph.generated.nodes.*
import org.apache.commons.lang3.StringUtils

import scala.collection.mutable

object AstCreatorHelper {

  private val TagsToKeepInFullName = List("<anonymous>", "<lambda>", "<global>", "<type>", "<extension>", "<wildcard>")
  private val ReturnTypeMatcher    = """^\(.*\)(->|:)(.+)$""".r
  private val ClosureSignatureMatcher = """^(\(.*\))\s*(.*)\s*->(.+)$""".r

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
    val normalizedTpe = StringUtils.normalizeSpace(rawType.stripSuffix(" ()")).stripSuffix(".Type")
    val tpe = stripGenerics(normalizedTpe) match {
      // Empty or problematic types
      case ""                   => Defines.Any
      case t if t.contains("?") => Defines.Any
      // Map builtin types
      case "Any"        => Defines.Any
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
      case t if t.startsWith("[") && t.endsWith("]")         => Defines.Array
      case ClosureSignatureMatcher(params, mods, returnType) =>
        // "throws" is the only modifier that swiftc keeps
        // so we have to restore it here to keep signatures
        // consistent between runs with compiler support and without.
        val m = if (mods.contains("throws")) { "throws" }
        else ""
        s"${Defines.Function}<$params$m->$returnType>".replace(" ", "")
      case t if t.contains("( ") => t.substring(0, t.indexOf("( "))
      // Default case
      case typeStr => typeStr
    }
    if (tpe.startsWith("@")) {
      tpe.substring(tpe.indexOf(" ") + 1)
    } else {
      tpe
    }
  }

  def isObjcCall(callMethodFullName: String): Boolean = {
    // TODO: there might be more prefixes to consider here, but these are the ones we have seen so far in our test codebases
    callMethodFullName.startsWith("cobjc") ||
    callMethodFullName.startsWith("(cs)") ||
    callMethodFullName.startsWith("(cswift)")
  }

}

trait AstCreatorHelper(implicit withSchemaValidation: ValidationMode) { this: AstCreator =>

  import AstCreatorHelper.*

  private val optionalBindingNames: mutable.HashMap[String, (baseName: String, fieldPath: List[String])] =
    mutable.HashMap.empty

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
    def isDefer(node: SwiftNode): Boolean =
      node
        .isInstanceOf[CodeBlockItemSyntax] && node.asInstanceOf[CodeBlockItemSyntax].item.isInstanceOf[DeferStmtSyntax]
    def isGuard(node: SwiftNode): Boolean =
      node
        .isInstanceOf[CodeBlockItemSyntax] && node.asInstanceOf[CodeBlockItemSyntax].item.isInstanceOf[GuardStmtSyntax]

    // Most blocks contain neither a defer nor a guard, so map directly and skip the partition/indexWhere
    // scans (which each allocate) in that common case.
    if (!elements.exists(node => isDefer(node) || isGuard(node))) {
      return elements.map(astForNode)
    }

    val (deferElements: List[SwiftNode], otherElements: List[SwiftNode]) = elements.partition(isDefer)
    val deferElementsAstsOrdered                                         = deferElements.reverse.map(astForNode)
    val indexOfGuardStmt                                                 = otherElements.indexWhere(isGuard)
    if (indexOfGuardStmt < 0) {
      otherElements.map(astForNode) ++ deferElementsAstsOrdered
    } else {
      val elementsBeforeGuard = otherElements.slice(0, indexOfGuardStmt)
      val guardStmt =
        otherElements(indexOfGuardStmt).asInstanceOf[CodeBlockItemSyntax].item.asInstanceOf[GuardStmtSyntax]
      val elementsAfterGuard = otherElements.slice(indexOfGuardStmt + 1, otherElements.size)

      // Apply optional binding desugaring for guard let
      // Create the block that will hold the unwrapped variables (blockNode argument is only used for location info)
      val thenBlockNode = if (elementsAfterGuard.nonEmpty) blockNode(elementsAfterGuard.head) else blockNode(guardStmt)

      val (conditionAst, unwrapAsts) = handleOptionalBindingConditions(
        guardStmt.conditions.children,
        // Handles Swift optional binding (guard-let) constructs.
        //
        // De-sugars `guard let x = foo() else { exit }` into:
        //   Condition:  { (<tmp>0 = foo()) != nil }
        //   Then block: { let x = <tmp>0 }
        //   Else block: { exit }
        //
        // For multiple bindings `guard let a = foo(), let b = bar() else { exit }`:
        //   Condition:  { ((<tmp>0 = foo()) != nil) && ((<tmp>1 = bar()) != nil) }
        //   Then block: { a = <tmp>0; b = <tmp>1 }
        //
        // For mixed cases with/without initializers `guard let a = foo(), let b else { exit }`:
        //   Condition:  { ((<tmp>0 = foo()) != nil) && (b != nil) }
        //   Then block: { a = <tmp>0 }
        onAllSimple = simpleBindings => {
          val bindingInfos = collectBindingInfos(simpleBindings)
          val condAst      = buildOptionalBindingCondition(guardStmt, bindingInfos)
          scope.pushNewBlockScope(thenBlockNode)
          localAstParentStack.push(thenBlockNode)
          val unwraps = buildUnwrapAssignments(bindingInfos)
          (condAst, unwraps)
        },
        // Handles partial optional binding desugaring with other conditions.
        //
        // De-sugars `guard let a = foo(), someCondition else { exit }` into:
        //   Condition:  { ((<tmp>0 = foo()) != nil) && someCondition }
        //   Then block: { let a = <tmp>0 }
        onPartial = (simpleBindings, tupleBindings, otherConditions) => {
          val bindingInfos = collectBindingInfos(simpleBindings)
          val condAst      = buildOptionalBindingCondition(guardStmt, bindingInfos, otherConditions)
          scope.pushNewBlockScope(thenBlockNode)
          localAstParentStack.push(thenBlockNode)
          val unwraps = buildUnwrapAssignments(bindingInfos) ++ tupleBindings.map(astForNode)
          (condAst, unwraps)
        },
        onStandard = () => {
          scope.pushNewBlockScope(thenBlockNode)
          localAstParentStack.push(thenBlockNode)
          val condAst = astForNode(guardStmt.conditions)
          (condAst, List.empty)
        }
      )

      val allThenChildren = unwrapAsts ++ astsForBlockElements(elementsAfterGuard) ++ deferElementsAstsOrdered

      // Closing the scope opened at the handleOptionalBindingConditions handler
      scope.popScope()
      localAstParentStack.pop()

      val thenAst = blockAst(thenBlockNode, allThenChildren)
      val elseAst = astForNode(guardStmt.body)

      val ifAst = ifThenElseAst(guardStmt, Some(conditionAst), thenAst, Some(elseAst))
      astsForBlockElements(elementsBeforeGuard) :+ ifAst
    }
  }

  /** Creates the AST for an identifier expression. Resolves the identifier to one of:
    *   - A variable used during optional binding de-sugaring
    *   - A `Self` type reference (enclosing type).
    *   - An implicit `self.member` field access when the identifier is a member of the enclosing type declaration.
    *   - An implicit `self.member` field access inferred from compiler metadata (swift-build mode only).
    *   - A standalone type reference when the compiler reports a `.Type` metatype (swift-build mode only).
    *   - A regular variable / captured-variable identifier as a fallback.
    */
  protected def astForIdentifier(node: SwiftNode): Ast = {
    val identifierName = code(node)
    val variableOption = scope.lookupVariable(identifierName)
    val isUnresolved   = variableOption.isEmpty

    identifierName match {
      // `Self` always refers to the enclosing type.
      case "Self" =>
        Ast(typeRefNode(node, "Self", fullNameOfEnclosingTypeDecl()))
      // Identifier found as a member of the surrounding type decl.
      // Swift does not allow accessing members of an outer class instance,
      // so we synthesize an implicit `self.<member>` (or `Self.<member>` in static contexts).
      case name if scope.variableIsInTypeDeclScope(name) && !optionalBindingNames.contains(identifierName) =>
        val tpe     = scope.typeDeclFullNameForMember(name).getOrElse(fullNameOfEnclosingTypeDecl())
        val callTpe = variableOption.map(_._2).getOrElse(Defines.Any)
        fieldAccessAstForSelf(node, name, tpe, callTpe)
      // In swift-build mode the compiler may know this identifier is a member even though
      // it is not yet visible in our scope. If the identifier is unresolved locally but
      // the compiler provides a declaration full name, treat it as an implicit self-access.
      case name
          if config.swiftBuild && isUnresolved &&
            fullnameProvider.declFullname(node).nonEmpty &&
            !optionalBindingNames.contains(identifierName) =>
        val tpe     = fullNameOfEnclosingTypeDecl()
        val callTpe = fullnameProvider.typeFullname(node).getOrElse(Defines.Any)
        registerType(callTpe)
        fieldAccessAstForSelf(node, name, tpe, callTpe)
      // In swift-build mode, an unresolved identifier whose compiler-reported type ends
      // with `.Type` is a reference to a type (e.g. `MyStruct` used as a value).
      case name
          if config.swiftBuild && isUnresolved &&
            fullnameProvider.typeFullnameRaw(node).exists(_.endsWith(".Type"))
            && !optionalBindingNames.contains(identifierName) =>
        val tpe = fullnameProvider.typeFullname(node).get
        registerType(tpe)
        Ast(typeRefNode(node, name, tpe))
      // Fallback: a regular variable or a captured variable from an outer scope.
      case name =>
        // For the de-sugaring of condition ASTs during `buildOptionalBindingCondition`,
        // we replace the identifier names of dependent optional binding variables to model
        // dependencies through the generated &&- and nil-check chain.
        optionalBindingNames.get(name) match {
          case Some((baseName, fieldPath)) if fieldPath.nonEmpty =>
            createFieldAccessChain(baseName, fieldPath, node)
          case other =>
            val resolvedName = other.map(_.baseName).getOrElse(name)
            val identNode    = identifierNode(node, resolvedName)
            val tpe = variableOption match {
              case Some((_, variableTypeName)) if variableTypeName != Defines.Any => variableTypeName
              case None if identNode.typeFullName != Defines.Any                  => identNode.typeFullName
              case _                                                              => Defines.Any
            }
            identNode.typeFullName = tpe
            scope.addVariableReference(resolvedName, identNode, tpe, EvaluationStrategies.BY_REFERENCE)
            Ast(identNode)
        }
    }
  }

  /** Builds a `self.<identifierName>` (or `Self.<identifierName>`) field-access AST. In a static method scope the
    * receiver is a `Self` type reference; otherwise it is a `self` identifier whose variable reference is registered in
    * the current scope.
    */
  private def fieldAccessAstForSelf(node: SwiftNode, identifierName: String, selfTpe: String, callTpe: String): Ast = {
    val selfNode = if (scope.isInStaticMethodScope) {
      typeRefNode(node, "Self", selfTpe)
    } else {
      val selfIdNode = identifierNode(node, "self", "self", selfTpe)
      scope.addVariableReference("self", selfIdNode, selfIdNode.typeFullName, EvaluationStrategies.BY_REFERENCE)
      selfIdNode
    }
    fieldAccessAst(node, node, Ast(selfNode), s"${selfNode.code}.$identifierName", identifierName, callTpe)
  }

  protected def registerType(typeFullName: String): Unit = {
    accumulator.registerType(typeFullName)
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

  private def transferEndOffsetToStartOffset(src: SwiftNode, dst: SwiftNode): SwiftNode = {
    dst.json("range")("startOffset") = src.json("range")("endOffset").num + 1
    dst
  }

  /** Try to get legacy node startOffset from modifiers since in older versions of swiftc (<6.2) it is not stored in the
    * JSON object.
    */
  private def legacyNode(node: FunctionDeclLike): Option[SwiftNode] = {
    (node match {
      case f: FunctionDeclSyntax      => f.modifiers.children.lastOption
      case a: AccessorDeclSyntax      => a.modifier
      case d: DeinitializerDeclSyntax => d.modifiers.children.lastOption
      case i: InitializerDeclSyntax   => i.modifiers.children.lastOption
      case s: SubscriptDeclSyntax     => s.modifiers.children.lastOption
      case c: ClosureExprSyntax       => None
    }).map(l => transferEndOffsetToStartOffset(l, node))
  }

  protected def methodInfoForFunctionDeclLike(node: FunctionDeclLike): MethodInfo = {
    val name = calcMethodName(node)

    // We need to copy the node range here because we might modify it when trying to get the legacy node for older swiftc versions.
    // (see: transferEndOffsetToStartOffset)
    // We want to keep the original range intact for the actual node.
    val nodeRange = node.asInstanceOf[SwiftNode].json("range").obj.copy()

    val methodInfo =
      fullnameProvider.declFullname(node).orElse(legacyNode(node).flatMap(fullnameProvider.declFullname)) match {
        case Some(fullNameWithSignature) =>
          val (fullName, signature) = methodInfoFromFullNameWithSignature(fullNameWithSignature)
          val returnType = node match {
            case _: DeinitializerDeclSyntax =>
              Defines.Void
            case _: InitializerDeclSyntax =>
              ReturnTypeMatcher.findFirstMatchIn(signature).map(_.group(2)).getOrElse(fullNameOfEnclosingTypeDecl())
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
              val returnType = fullNameOfEnclosingTypeDecl()
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

    node.json("range") = nodeRange
    methodInfo
  }

  case class MethodInfo(name: String, fullName: String, signature: String, returnType: String) {
    val fullNameAndSignature: String    = s"$fullName:$signature"
    val fullNameAndSignatureExt: String = MethodInfo.fullNameToExtensionFullName(fullNameAndSignature, name)
  }
  object MethodInfo {
    def fullNameToExtensionFullName(fullName: String, name: String): String = {
      val (replacementRegex, replacement) = if (fullName.contains(".subscript:")) {
        (".subscript:", s"<extension>.subscript:")
      } else if (fullName.contains(".subscript.getter:")) {
        (".subscript.getter:", s"<extension>.subscript.getter:")
      } else if (fullName.contains(".subscript.setter:")) {
        (".subscript.setter:", s"<extension>.subscript.setter:")
      } else {
        (s".$name:", s"<extension>.$name:")
      }
      fullName.replace(replacementRegex, replacement)
    }
  }

  case class TypeInfo(name: String, fullName: String)

  protected def methodInfoForAccessorDecl(
    node: AccessorDeclSyntax,
    variableName: String,
    tpe: String,
    fullNameSubscriptPrefix: String = ""
  ): MethodInfo = {
    val accessorName = code(node.accessorSpecifier)
    val namePrefix   = if (variableName.nonEmpty) s"$variableName." else ""
    val name = accessorName match {
      case "set" => s"${namePrefix}setter"
      case "get" => s"${namePrefix}getter"
      case other => s"$namePrefix$other"
    }

    fullnameProvider.declFullname(node) match {
      case Some(fullNameWithSignature) =>
        var fullName  = fullNameWithSignature
        var signature = tpe
        if (fullNameWithSignature.contains(":")) {
          fullName = fullNameWithSignature.substring(0, fullNameWithSignature.lastIndexOf(":"))
          signature = fullNameWithSignature.substring(fullNameWithSignature.lastIndexOf(":") + 1)
        }
        MethodInfo(name, fullName, signature, tpe)
      case None =>
        val (methodName, methodFullName) = calcNameAndFullName(name, fullNameSubscriptPrefix)
        registerType(tpe)
        MethodInfo(methodName, methodFullName, tpe, tpe)
    }
  }

  protected def methodInfoForAccessorDecl(node: PatternBindingSyntax, variableName: String, tpe: String): MethodInfo = {
    fullnameProvider.declFullname(node) match {
      case Some(fullNameWithSignature) =>
        var fullName  = fullNameWithSignature
        var signature = tpe
        if (fullNameWithSignature.contains(":")) {
          fullName = fullNameWithSignature.substring(0, fullNameWithSignature.lastIndexOf(":"))
          signature = fullNameWithSignature.substring(fullNameWithSignature.lastIndexOf(":") + 1)
        }
        MethodInfo(variableName, fullName, signature, tpe)
      case None =>
        val (methodName, methodFullName) = calcNameAndFullName(variableName)
        registerType(tpe)
        MethodInfo(methodName, methodFullName, tpe, tpe)
    }
  }

  protected def typeNameInfoForDeclSyntax(node: DeclSyntax): TypeInfo = {
    val name = typeNameForDeclSyntax(node)
    typeNameInfoForNode(node, name)
  }

  private def nameForTypeSyntax(node: TypeSyntax): String = {
    val name = node match {
      case _: ArrayTypeSyntax                       => "array-type"
      case a: AttributedTypeSyntax                  => nameForTypeSyntax(a.baseType)
      case _: ClassRestrictionTypeSyntax            => "class-restriction-type"
      case _: CompositionTypeSyntax                 => "composition-type"
      case _: DictionaryTypeSyntax                  => "dictionary-type"
      case _: FunctionTypeSyntax                    => Defines.Function
      case id: IdentifierTypeSyntax                 => code(id.name)
      case _: InlineArrayTypeSyntax                 => "inline-array-type"
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

  protected def simpleTypeNameForTypeSyntax(node: TypeSyntax): String = {
    node match {
      case _: ArrayTypeSyntax                       => Defines.Array
      case a: AttributedTypeSyntax                  => simpleTypeNameForTypeSyntax(a.baseType)
      case _: ClassRestrictionTypeSyntax            => Defines.Class
      case _: CompositionTypeSyntax                 => scopeLocalUniqueName("composition-type")
      case _: DictionaryTypeSyntax                  => Defines.Dictionary
      case _: FunctionTypeSyntax                    => Defines.Function
      case id: IdentifierTypeSyntax                 => scopeLocalUniqueName(cleanType(code(id.name)))
      case _: InlineArrayTypeSyntax                 => Defines.Array
      case w: ImplicitlyUnwrappedOptionalTypeSyntax => simpleTypeNameForTypeSyntax(w.wrappedType)
      case m: MemberTypeSyntax                      => scopeLocalUniqueName(cleanType(code(m)))
      case m: MetatypeTypeSyntax                    => simpleTypeNameForTypeSyntax(m.baseType)
      case _: MissingTypeSyntax                     => scopeLocalUniqueName("missing-type")
      case n: NamedOpaqueReturnTypeSyntax           => simpleTypeNameForTypeSyntax(n.`type`)
      case o: OptionalTypeSyntax                    => simpleTypeNameForTypeSyntax(o.wrappedType)
      case p: PackElementTypeSyntax                 => simpleTypeNameForTypeSyntax(p.pack)
      case p: PackExpansionTypeSyntax               => simpleTypeNameForTypeSyntax(p.repetitionPattern)
      case s: SomeOrAnyTypeSyntax                   => simpleTypeNameForTypeSyntax(s.constraint)
      case s: SuppressedTypeSyntax                  => simpleTypeNameForTypeSyntax(s.`type`)
      case _: TupleTypeSyntax                       => Defines.Tuple
    }
  }

  protected def typeNameInfoForTypeSyntax(node: TypeSyntax): TypeInfo = {
    val name = nameForTypeSyntax(node)
    typeNameInfoForNode(node, name)
  }

  private def typeNameInfoForNode(node: SwiftNode, name: String): TypeInfo = {
    fullnameProvider.declFullname(node) match {
      case Some(declFullname) =>
        val cleanedFullName = if (declFullname.contains("(")) {
          val fullName = declFullname.substring(0, declFullname.indexOf("("))
          if (fullName.contains(".")) {
            fullName.substring(0, fullName.lastIndexOf("."))
          } else {
            fullName
          }
        } else declFullname
        if (!node.isInstanceOf[ExtensionDeclSyntax]) {
          registerType(cleanedFullName)
        }
        TypeInfo(name, cleanedFullName)
      case None =>
        val (_, declFullname) = calcNameAndFullName(name)
        if (!node.isInstanceOf[ExtensionDeclSyntax]) {
          registerType(declFullname)
        }
        TypeInfo(name, declFullname)
    }
  }

  private def calcNameAndFullName(name: String, fullNameSubscriptPrefix: String = ""): (String, String) = {
    val fullNamePrefix = s"${parserResult.filename}:${scope.computeScopePath}"
    val methodFullNameWithSubscriptPrefix = if (fullNameSubscriptPrefix.nonEmpty) {
      s"$fullNamePrefix.$fullNameSubscriptPrefix.$name"
    } else {
      s"$fullNamePrefix.$name"
    }
    (name, methodFullNameWithSubscriptPrefix)
  }

  private def calcMethodName(func: SwiftNode): String = {
    func match {
      case f: FunctionDeclSyntax      => cleanName(code(f.name))
      case a: AccessorDeclSyntax      => code(a.accessorSpecifier)
      case d: DeinitializerDeclSyntax => "deinit"
      case i: InitializerDeclSyntax   => "init"
      case s: SubscriptDeclSyntax     => "subscript"
      case _                          => nextClosureName()
    }
  }

  /** Checks if a pattern is tuple-like (direct TuplePatternSyntax or ExpressionPatternSyntax wrapping TupleExprSyntax).
    * Used to detect tuple patterns in optional binding conditions.
    */
  protected def isTupleLikePattern(pattern: PatternSyntax): Boolean = pattern match {
    case _: TuplePatternSyntax                                                      => true
    case ep: ExpressionPatternSyntax if ep.expression.isInstanceOf[TupleExprSyntax] => true
    case _                                                                          => false
  }

  /** Information about an optional binding for desugaring if-let/while-let constructs.
    *
    * Contains the CPG-level names for optional binding desugaring. The localName is the unwrapped variable name that
    * appears in the then/body block. The tmpName (when present) holds the optional value from the initializer and is
    * used for the nil check in the condition block, ensuring single evaluation of the initializer expression.
    *
    * @param localName
    *   CPG name for the unwrapped variable in the then/body block (e.g., "a" in `if let a = foo()`)
    * @param tmpName
    *   CPG name for temporary holding the optional value in condition (e.g., "<tmp>0" in the nil check)
    * @param binding
    *   The source-level OptionalBindingConditionSyntax node
    * @param isWildcard
    *   True if the pattern is a wildcard (e.g., `if let _ = foo()`), requiring generated name
    * @param tuplePattern
    *   The tuple pattern for tuple optional bindings (e.g., `(a, b)` in `if let (a, b) = foo()`); None for simple
    *   bindings
    */
  protected case class BindingInfo(
    localName: String,
    tmpName: Option[String],
    binding: OptionalBindingConditionSyntax,
    isWildcard: Boolean,
    tuplePattern: Option[PatternSyntax] = None
  )

  /** Recursively walks a tuple pattern or tuple expression and registers every leaf variable name into
    * [[optionalBindingNames]] so that identifier resolution in [[astForIdentifier]] does not wrongly treat those names
    * as implicit `self.member` accesses.
    *
    * Handles:
    *   - `TuplePatternSyntax` — recurse into each element's `.pattern`, accumulating field index path
    *   - `ExpressionPatternSyntax` wrapping `TupleExprSyntax` — recurse into each element's `.expression`
    *   - `IdentifierPatternSyntax` — register `code(identifier)` → `(tmpName, fieldPath)`
    *   - `ValueBindingPatternSyntax(IdentifierPatternSyntax)` — register inner identifier name → `(tmpName, fieldPath)`
    *   - `WildcardPatternSyntax` — skip (wildcard, nothing to register)
    *
    * @param pattern
    *   The top-level pattern from the binding (either a `TuplePatternSyntax` or `ExpressionPatternSyntax` wrapping a
    *   `TupleExprSyntax`)
    * @param tmpName
    *   The generated temporary name to associate with each bound variable
    * @param fieldPath
    *   The accumulated list of field index strings representing the path from `tmpName` to the leaf variable
    */
  private def registerTuplePatternBindings(
    pattern: PatternSyntax,
    tmpName: String,
    fieldPath: List[String] = List.empty
  ): Unit = {
    def walkExpr(expr: ExprSyntax, currentPath: List[String]): Unit = expr match {
      case inner: TupleExprSyntax =>
        inner.elements.children.zipWithIndex.foreach { case (elem, idx) =>
          walkExpr(elem.expression, currentPath :+ idx.toString)
        }
      case patExpr: PatternExprSyntax =>
        walkPattern(patExpr.pattern, currentPath)
      case declRef: DeclReferenceExprSyntax =>
        optionalBindingNames.put(code(declRef), (tmpName, currentPath))
      case _: DiscardAssignmentExprSyntax =>
      // skip wildcards
      case other =>
        // Unreachable in valid Swift tuple patterns; over-registers at worst, never under-registers
        optionalBindingNames.put(code(other), (tmpName, currentPath))
    }

    def walkPattern(pat: PatternSyntax, currentPath: List[String]): Unit = pat match {
      case tuplePat: TuplePatternSyntax =>
        tuplePat.elements.children.zipWithIndex.foreach { case (elem, idx) =>
          walkPattern(elem.pattern, currentPath :+ idx.toString)
        }
      case ep: ExpressionPatternSyntax =>
        walkExpr(ep.expression, currentPath)
      case ident: IdentifierPatternSyntax =>
        optionalBindingNames.put(code(ident.identifier), (tmpName, currentPath))
      case vb: ValueBindingPatternSyntax =>
        vb.pattern match {
          case ident: IdentifierPatternSyntax =>
            optionalBindingNames.put(code(ident.identifier), (tmpName, currentPath))
          case inner =>
            walkPattern(inner, currentPath)
        }
      case _: WildcardPatternSyntax =>
      // skip wildcards
      case other =>
        logger.debug(s"Unhandled pattern type in tuple binding: ${other.getClass.getSimpleName}")
    }

    walkPattern(pattern, fieldPath)
  }

  protected def collectBindingInfos(bindings: Seq[OptionalBindingConditionSyntax]): Seq[BindingInfo] = {
    bindings.map { binding =>
      if (isTupleLikePattern(binding.pattern)) {
        val tmpName = binding.initializer.map(_ => scopeLocalUniqueName("tmp"))
        if (tmpName.isDefined) {
          registerTuplePatternBindings(binding.pattern, tmpName.get)
        }
        BindingInfo(
          scopeLocalUniqueName("tupleWildcard"),
          tmpName,
          binding,
          isWildcard = true,
          tuplePattern = Some(binding.pattern)
        )
      } else {
        val (localName, isWildcard) = binding.pattern match {
          case ident: IdentifierPatternSyntax => (code(ident.identifier), false)
          case _                              => (scopeLocalUniqueName("wildcard"), true)
        }
        val tmpName = binding.initializer.map(_ => scopeLocalUniqueName("tmp"))
        if (!isWildcard && tmpName.isDefined) {
          optionalBindingNames.put(localName, (tmpName.get, List.empty))
        }
        BindingInfo(localName, tmpName, binding, isWildcard, tuplePattern = None)
      }
    }
  }

  protected def handleOptionalBindingConditions[T](
    conditions: Iterable[ConditionElementSyntax],
    onAllSimple: Seq[OptionalBindingConditionSyntax] => T,
    onPartial: (
      Seq[OptionalBindingConditionSyntax],
      Seq[OptionalBindingConditionSyntax],
      Seq[ConditionElementSyntax]
    ) => T,
    onStandard: () => T
  ): T = {
    val conditionsSeq = conditions.toSeq
    val optionalBindings = conditionsSeq.collect {
      case condElem if condElem.condition.isInstanceOf[OptionalBindingConditionSyntax] =>
        condElem.condition.asInstanceOf[OptionalBindingConditionSyntax]
    }

    val (simpleBindings, tupleBindings) = optionalBindings.partition(binding => !isTupleLikePattern(binding.pattern))
    val otherConditions =
      conditionsSeq.filterNot(condElem => condElem.condition.isInstanceOf[OptionalBindingConditionSyntax])

    val allBindings = simpleBindings ++ tupleBindings

    if (allBindings.isEmpty) {
      // No optional bindings at all — standard handling
      onStandard()
    } else if (otherConditions.isEmpty) {
      // All conditions are optional bindings (simple and tuple-like)
      onAllSimple(allBindings)
    } else {
      // Optional bindings plus other conditions (e.g. boolean guards)
      onPartial(allBindings, Seq.empty, otherConditions)
    }
  }

  /** Combines multiple nil check ASTs with logical AND operator. If only one check exists, returns it directly.
    *
    * @param node
    *   The control structure node for creating operator nodes
    * @param nilCheckAsts
    *   The nil check ASTs to combine
    * @return
    *   Single AST representing all checks combined with &&
    */
  private def combineNilChecksWithAnd(node: SwiftNode, nilCheckAsts: Seq[Ast]): Ast = {
    if (nilCheckAsts.size == 1) {
      nilCheckAsts.head
    } else {
      nilCheckAsts.reduce { (left, right) =>
        val leftCode    = left.root.map(codeOf).getOrElse("")
        val rightCode   = right.root.map(codeOf).getOrElse("")
        val andCode     = s"($leftCode) && ($rightCode)"
        val andCallNode = createStaticCallNode(node, andCode, Operators.logicalAnd, Operators.logicalAnd, Defines.Bool)
        callAst(andCallNode, List(left, right))
      }
    }
  }

  /** Builds the condition AST for optional binding constructs (if-let/while-let). If any binding has an initializer,
    * creates a block with temp variable assignments and nil checks. Otherwise, creates a simple combined nil check.
    *
    * @param node
    *   The control structure node (IfExprSyntax or WhileStmtSyntax)
    * @param bindingInfos
    *   Information about each optional binding
    * @param additionalConditions
    *   Additional condition elements to AND with the nil checks
    * @return
    *   Condition AST (either block or direct nil check)
    */
  protected def buildOptionalBindingCondition(
    node: SwiftNode,
    bindingInfos: Seq[BindingInfo],
    additionalConditions: Seq[ConditionElementSyntax] = Seq.empty
  ): Ast = {
    val hasAnyInitializer = bindingInfos.exists(_.tmpName.isDefined)

    val conditionAst = if (hasAnyInitializer) {
      bindingInfos.foreach { info =>
        info.tmpName.foreach { tmpName =>
          val tmpLocalNode = localNode(info.binding, tmpName, tmpName, Defines.Any).order(0)
          diffGraph.addEdge(localAstParentStack.head, tmpLocalNode, EdgeTypes.AST)
          scope.addVariable(tmpName, tmpLocalNode, Defines.Any, VariableScopeManager.ScopeType.BlockScope)
        }
      }

      val condBlockNode = blockNode(node)
      scope.pushNewBlockScope(condBlockNode)
      localAstParentStack.push(condBlockNode)

      val nilCheckAsts = bindingInfos.map { info =>
        info.tmpName match {
          case Some(tmpName) =>
            val tmpIdentNode = identifierNode(info.binding, tmpName, tmpName, Defines.Any)
            scope.addVariableReference(tmpName, tmpIdentNode, Defines.Any, EvaluationStrategies.BY_REFERENCE)
            val initAst = astForNode(info.binding.initializer.get.value)
            val assignAst = createAssignmentCallAst(
              info.binding,
              Ast(tmpIdentNode),
              initAst,
              s"$tmpName = ${codeOf(initAst.root.get)}"
            )

            val nilNode = literalNode(info.binding, "nil", Option(Defines.Nil))
            val checkCallNode = createStaticCallNode(
              info.binding,
              s"($tmpName = ${codeOf(initAst.root.get)}) != nil",
              Operators.notEquals,
              Operators.notEquals,
              Defines.Bool
            )
            callAst(checkCallNode, List(assignAst, Ast(nilNode)))
          case None =>
            val patternAst = astForNode(info.binding.pattern)
            val nilNode    = literalNode(info.binding, "nil", Option(Defines.Nil))
            val checkCallNode = createStaticCallNode(
              info.binding,
              s"${info.localName} != nil",
              Operators.notEquals,
              Operators.notEquals,
              Defines.Bool
            )
            callAst(checkCallNode, List(patternAst, Ast(nilNode)))
        }
      }

      val additionalConditionAsts = additionalConditions.map(condElem => astForNode(condElem.condition))
      val allChecks               = nilCheckAsts ++ additionalConditionAsts
      val combinedCheckAst        = combineNilChecksWithAnd(node, allChecks)

      scope.popScope()
      localAstParentStack.pop()

      blockAst(condBlockNode, List(combinedCheckAst))
    } else {
      val nilCheckAsts = bindingInfos.map { info =>
        val patternAst = astForNode(info.binding.pattern)
        val nilNode    = literalNode(info.binding, "nil", Option(Defines.Nil))
        val checkCallNode = createStaticCallNode(
          info.binding,
          s"${info.localName} != nil",
          Operators.notEquals,
          Operators.notEquals,
          Defines.Bool
        )
        callAst(checkCallNode, List(patternAst, Ast(nilNode)))
      }

      val additionalConditionAsts = additionalConditions.map(condElem => astForNode(condElem.condition))
      val allChecks               = nilCheckAsts ++ additionalConditionAsts
      combineNilChecksWithAnd(node, allChecks)
    }
    optionalBindingNames.clear()
    conditionAst
  }

  /** Builds the body AST with optional unwrapping assignments prepended. For bindings with initializers, creates locals
    * and unwrapping assignments in the body block.
    *
    * @param bodyNode
    *   The body syntax node for creating the block
    * @param bodyStatements
    *   The statements to include in the body
    * @param bindingInfos
    *   Information about each optional binding
    * @return
    *   Body AST with unwrapping assignments (if needed) followed by original body statements
    */
  protected def buildBodyWithUnwrapping(
    bodyNode: SwiftNode,
    bodyStatements: Iterable[SwiftNode],
    bindingInfos: Seq[BindingInfo]
  ): Ast = {
    val bindingsWithInitializer = bindingInfos.filter(info => info.tmpName.isDefined && !info.isWildcard)
    val tupleBindingsWithInitializer =
      bindingInfos.filter(info => info.tmpName.isDefined && info.tuplePattern.isDefined)
    val hasUnwrapping = bindingsWithInitializer.nonEmpty || tupleBindingsWithInitializer.nonEmpty

    if (hasUnwrapping) {
      val bodyBlockNode = blockNode(bodyNode)
      scope.pushNewBlockScope(bodyBlockNode)
      localAstParentStack.push(bodyBlockNode)

      val unwrapAsts = bindingInfos.filter(_.tmpName.isDefined).flatMap { info =>
        unwrapAssignmentsForBindingInfo(info, bodyBlockNode)
      }

      val bodyAsts = astsForBlockElements(bodyStatements.toList)

      scope.popScope()
      localAstParentStack.pop()

      if (unwrapAsts.isEmpty && bodyAsts.isEmpty) {
        Ast(bodyBlockNode)
      } else {
        blockAst(bodyBlockNode, unwrapAsts.toList ++ bodyAsts)
      }
    } else {
      astForNode(bodyNode)
    }
  }

  private def buildUnwrapAssignments(bindingInfos: Seq[BindingInfo]): List[Ast] = {
    bindingInfos
      .filter(_.tmpName.isDefined)
      .flatMap(info => unwrapAssignmentsForBindingInfo(info, localAstParentStack.head))
      .toList
  }

  private def unwrapAssignmentsForBindingInfo(info: BindingInfo, localParent: NewBlock): List[Ast] = {
    info.tuplePattern match {
      case Some(tp: TuplePatternSyntax) =>
        astsForBindingTuplePattern(tp, info.tmpName.get, List.empty, info.binding)
      case Some(ep: ExpressionPatternSyntax) if ep.expression.isInstanceOf[TupleExprSyntax] =>
        astsForBindingTupleExpr(ep.expression.asInstanceOf[TupleExprSyntax], info.tmpName.get, List.empty, info.binding)
      case _ if !info.isWildcard =>
        val binding = info.binding
        val tmpName = info.tmpName.get

        val typeFullName =
          binding.typeAnnotation.map(typeAnn => AstCreatorHelper.cleanType(code(typeAnn.`type`))).getOrElse(Defines.Any)
        val kind = code(binding.bindingSpecifier)
        val scopeType =
          if (kind == "let") VariableScopeManager.ScopeType.BlockScope
          else VariableScopeManager.ScopeType.MethodScope

        val tpeFromTypeMap = fullnameProvider.typeFullname(binding.pattern).getOrElse(typeFullName)
        registerType(tpeFromTypeMap)
        val localNode_ = localNode(binding, info.localName, info.localName, tpeFromTypeMap).order(0)
        scope.addVariable(info.localName, localNode_, tpeFromTypeMap, scopeType)
        diffGraph.addEdge(localParent, localNode_, EdgeTypes.AST)

        val localIdentNode = identifierNode(binding, info.localName, info.localName, tpeFromTypeMap)
        scope.addVariableReference(info.localName, localIdentNode, tpeFromTypeMap, EvaluationStrategies.BY_REFERENCE)

        val tmpIdent = identifierNode(binding, tmpName, tmpName, Defines.Any)
        scope.addVariableReference(tmpName, tmpIdent, Defines.Any, EvaluationStrategies.BY_REFERENCE)
        List(createAssignmentCallAst(binding, Ast(localIdentNode), Ast(tmpIdent), s"${info.localName} = $tmpName"))
      case _ => List.empty
    }
  }

}
