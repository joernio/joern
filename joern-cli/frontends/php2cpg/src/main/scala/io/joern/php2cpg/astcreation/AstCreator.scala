package io.joern.php2cpg.astcreation

import io.joern.php2cpg.astcreation.AstCreator.{NameConstants, TypeConstants}
import io.joern.php2cpg.parser.Domain.*
import io.joern.php2cpg.passes.SymbolSummaryPass.SymbolSummary
import io.joern.php2cpg.utils.{NamespaceScope, Scope}
import io.joern.x2cpg.Ast.storeInDiffGraph
import io.joern.x2cpg.utils.AstPropertiesUtil.RootProperties
import io.joern.x2cpg.{Ast, AstCreatorBase, Defines, ValidationMode}
import io.shiftleft.codepropertygraph.generated.*
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.types.structure.NamespaceTraversal
import io.shiftleft.utils.IOUtils
import org.slf4j.{Logger, LoggerFactory}

import java.nio.file.Path

class AstCreator(
  protected val relativeFileName: String,
  fileName: String,
  phpAst: PhpFile,
  disableFileContent: Boolean,
  summary: Map[String, Seq[SymbolSummary]]
)(implicit withSchemaValidation: ValidationMode)
    extends AstCreatorBase[PhpNode, AstCreator](relativeFileName)
    with AstCreatorHelper(disableFileContent)
    with AstForExpressionsCreator
    with AstForControlStructuresCreator
    with AstForDeclarationsCreator
    with AstForFunctionsCreator
    with AstForTypesCreator {

  protected val logger: Logger = LoggerFactory.getLogger(AstCreator.getClass)
  protected val scope          = new Scope(summary, () => nextClosureName())
  protected var fileContent    = Option.empty[String]

  override def createAst(): DiffGraphBuilder = {
    if (!disableFileContent) {
      fileContent = Option(IOUtils.readEntireFile(Path.of(fileName)))
    }

    val ast = astForPhpFile(phpAst)
    storeInDiffGraph(ast, diffGraph)
    diffGraph
  }

  private def flattenGlobalNamespaceStmt(stmt: PhpStmt): List[PhpStmt] = {
    stmt match {
      case namespace: PhpNamespaceStmt if namespace.name.isEmpty =>
        namespace.stmts

      case _ => stmt :: Nil
    }
  }

  private def globalMethodDeclStmt(file: PhpFile, bodyStmts: List[PhpStmt]): PhpMethodDecl = {
    val modifiersList = List(ModifierTypes.VIRTUAL, ModifierTypes.PUBLIC, ModifierTypes.STATIC, ModifierTypes.MODULE)
    // lineNumber and lineNumberEnd must be set to some value to avoid back-end crashes, so default to 0 if
    // the file is empty.
    val lineNumber    = bodyStmts.headOption.flatMap(line).getOrElse(0)
    val lineNumberEnd = bodyStmts.lastOption.flatMap(lineEnd).getOrElse(0)
    PhpMethodDecl(
      name = PhpNameExpr(NamespaceTraversal.globalNamespaceName, file.attributes),
      params = Nil,
      modifiers = modifiersList,
      returnType = None,
      stmts = bodyStmts,
      returnByRef = false,
      namespacedName = None,
      isClassMethod = false,
      attributes = file.attributes.copy(Option(lineNumber), Option(lineNumberEnd)),
      attributeGroups = Seq.empty[PhpAttributeGroup]
    )
  }

  private def astForPhpFile(file: PhpFile): Ast = {
    val fileNode = NewFile().name(relativeFileName)
    fileContent.foreach(fileNode.content(_))

    scope.pushNewScope(NamespaceScope(globalNamespace, globalNamespace.fullName))

    val (globalDeclStmts, globalMethodStmts) =
      file.children.flatMap(flattenGlobalNamespaceStmt).partition(_.isInstanceOf[PhpConstStmt])

    val globalMethodStmt = globalMethodDeclStmt(file, globalMethodStmts)

    val globalTypeDeclStmt = PhpClassLikeStmt(
      name = Some(PhpNameExpr(globalNamespace.name, file.attributes)),
      modifiers = Nil,
      extendsNames = Nil,
      implementedInterfaces = Nil,
      stmts = globalDeclStmts.appended(globalMethodStmt),
      classLikeType = ClassLikeTypes.Class,
      scalarType = None,
      hasConstructor = false,
      attributes = file.attributes,
      Seq.empty[PhpAttributeGroup]
    )

    val globalTypeDeclAst = astForGlobalDecl(globalTypeDeclStmt, globalTypeDeclStmt.name.get)

    scope.popScope() // globalNamespace

    Ast(fileNode).withChild(Ast(globalNamespace).withChild(globalTypeDeclAst))
  }

  protected def astsForStmt(stmt: PhpStmt): List[Ast] = {
    stmt match {
      case echoStmt: PhpEchoStmt           => astForEchoStmt(echoStmt) :: Nil
      case methodDecl: PhpMethodDecl       => astForMethodDecl(methodDecl) :: Nil
      case expr: PhpExpr                   => astForExpr(expr) :: Nil
      case breakStmt: PhpBreakStmt         => astForBreakStmt(breakStmt) :: Nil
      case contStmt: PhpContinueStmt       => astForContinueStmt(contStmt) :: Nil
      case whileStmt: PhpWhileStmt         => astForWhileStmt(whileStmt) :: Nil
      case doStmt: PhpDoStmt               => astForDoStmt(doStmt) :: Nil
      case forStmt: PhpForStmt             => astForForStmt(forStmt) :: Nil
      case ifStmt: PhpIfStmt               => astForIfStmt(ifStmt) :: Nil
      case switchStmt: PhpSwitchStmt       => astForSwitchStmt(switchStmt) :: Nil
      case tryStmt: PhpTryStmt             => astForTryStmt(tryStmt) :: Nil
      case returnStmt: PhpReturnStmt       => astForReturnStmt(returnStmt) :: Nil
      case classLikeStmt: PhpClassLikeStmt => astForClassLikeStmt(classLikeStmt)
      case gotoStmt: PhpGotoStmt           => astForGotoStmt(gotoStmt) :: Nil
      case labelStmt: PhpLabelStmt         => astForLabelStmt(labelStmt) :: Nil
      case namespace: PhpNamespaceStmt     => astForNamespaceStmt(namespace) :: Nil
      case declareStmt: PhpDeclareStmt     => astForDeclareStmt(declareStmt) :: Nil
      case _: NopStmt                      => Nil // TODO This'll need to be updated when comments are added.
      case haltStmt: PhpHaltCompilerStmt   => astForHaltCompilerStmt(haltStmt) :: Nil
      case unsetStmt: PhpUnsetStmt         => astForUnsetStmt(unsetStmt) :: Nil
      case globalStmt: PhpGlobalStmt       => astForGlobalStmt(globalStmt)
      case useStmt: PhpUseStmt             => astForUseStmt(useStmt) :: Nil
      case groupUseStmt: PhpGroupUseStmt   => astForGroupUseStmt(groupUseStmt) :: Nil
      case foreachStmt: PhpForeachStmt     => astForForeachStmt(foreachStmt) :: Nil
      case traitUseStmt: PhpTraitUseStmt   => astforTraitUseStmt(traitUseStmt) :: Nil
      case enumCase: PhpEnumCaseStmt       => astForEnumCase(enumCase) :: Nil
      case staticStmt: PhpStaticStmt       => astsForStaticStmt(staticStmt)
      case unhandled =>
        logger.error(s"Unhandled stmt $unhandled in $relativeFileName")
        ???
    }
  }

  private def astForEchoStmt(echoStmt: PhpEchoStmt): Ast = {
    val args     = echoStmt.exprs.map(astForExpr)
    val code     = s"echo ${args.map(_.rootCodeOrEmpty).mkString(",")}"
    val callNode = operatorCallNode(echoStmt, code, "echo", None)
    callAst(callNode, args)
  }

  private def astForNamespaceStmt(stmt: PhpNamespaceStmt): Ast = {
    val name     = stmt.name.map(_.name).getOrElse(NameConstants.Unknown)
    val fullName = s"$relativeFileName:$name"

    val namespaceBlock = NewNamespaceBlock()
      .name(name)
      .fullName(fullName)

    scope.pushNewScope(NamespaceScope(namespaceBlock, namespaceBlock.fullName))
    val bodyStmts = astsForClassLikeBody(stmt, stmt.stmts, createDefaultConstructor = false)
    scope.popScope()

    Ast(namespaceBlock).withChildren(bodyStmts)
  }

  private def astForHaltCompilerStmt(stmt: PhpHaltCompilerStmt): Ast = {
    val call =
      operatorCallNode(stmt, s"${NameConstants.HaltCompiler}()", NameConstants.HaltCompiler, Some(TypeConstants.Void))

    Ast(call)
  }

  private def astForUnsetStmt(stmt: PhpUnsetStmt): Ast = {
    val name = PhpOperators.unset
    val args = stmt.vars.map(astForExpr)
    val code = s"$name(${args.map(_.rootCodeOrEmpty).mkString(", ")})"
    val callNode = operatorCallNode(stmt, code, name, Some(TypeConstants.Void))
      .methodFullName(PhpOperators.unset)
    callAst(callNode, args)
  }

  private def astForGlobalStmt(stmt: PhpGlobalStmt): List[Ast] = {
    val surroundingIter    = scope.getSurroundingMethods.drop(1).iterator // drop first to ignore global method
    val surroundingMethods = scope.getSurroundingMethods.dropRight(1)     // drop last to ignore innermost method

    surroundingMethods.foreach { currentMethod =>
      val innerMethodScope = surroundingIter.next()
      val innerMethodNode  = innerMethodScope.methodNode
      val innerMethodRef   = innerMethodScope.methodRefNode
      innerMethodRef match {
        case Some(methodRef) =>
          scope.getMethodRef(innerMethodNode.fullName) match {
            case None =>
              diffGraph.addNode(methodRef)
              diffGraph.addEdge(currentMethod.bodyNode, methodRef, EdgeTypes.AST)
              scope.addMethodRef(innerMethodNode.fullName, methodRef)
            case _ =>
          }

          stmt.vars.foreach {
            case PhpVariable(name: PhpNameExpr, _) =>
              val closureBindingId = s"$relativeFileName:${innerMethodNode.fullName}:${name.name}"
              val closureLocal     = localNode(stmt, name.name, name.name, Defines.Any, Option(closureBindingId))

              val closureBindingNode = NewClosureBinding()
                .closureBindingId(closureBindingId)
                .evaluationStrategy(EvaluationStrategies.BY_SHARING)

              scope.lookupVariable(name.name) match {
                case Some(refLocal) => diffGraph.addEdge(closureBindingNode, refLocal, EdgeTypes.REF)
                case _              => // do nothing
              }

              scope.addVariableToMethodScope(closureLocal.name, closureLocal, innerMethodNode.fullName) match {
                case Some(ms) => diffGraph.addEdge(ms.bodyNode, closureLocal, EdgeTypes.AST)
                case _        => // do nothing
              }

              diffGraph.addNode(closureBindingNode)
              diffGraph.addEdge(methodRef, closureBindingNode, EdgeTypes.CAPTURE)
            case x =>
              logger.warn(s"Unexpected variable type ${x.getClass} found")
          }
        case None =>
          logger.warn(s"No methodRef found for capturing global variable in method ${innerMethodNode.fullName}")
      }
    }

    stmt.vars.map(astForExpr)
  }

  private def astForUseStmt(stmt: PhpUseStmt): Ast = {
    val imports = stmt.uses.map(astForUseUse(_))
    scope.useImport(imports.flatMap(_.nodes).collect { case x: NewImport => x })
    wrapMultipleInBlock(imports, line(stmt))
  }

  private def astForGroupUseStmt(stmt: PhpGroupUseStmt): Ast = {
    val groupPrefix = s"${stmt.prefix.name}\\"
    val imports     = stmt.uses.map(astForUseUse(_, groupPrefix))
    scope.useImport(imports.flatMap(_.nodes).collect { case x: NewImport => x })
    wrapMultipleInBlock(imports, line(stmt))
  }

  private def astforTraitUseStmt(stmt: PhpTraitUseStmt): Ast = {
    // TODO Actually implement this
    logger.debug(
      s"Trait use statement encountered. This is not yet supported. Location: $relativeFileName:${line(stmt)}"
    )
    Ast(unknownNode(stmt, code(stmt)))
  }

  private def astForUseUse(stmt: PhpUseUse, namePrefix: String = ""): Ast = {
    val originalName = s"$namePrefix${stmt.originalName.name}"
    val aliasCode    = stmt.alias.map(alias => s" as ${alias.name}").getOrElse("")
    val typeCode = stmt.useType match {
      case PhpUseType.Function => s"function "
      case PhpUseType.Constant => s"const "
      case _                   => ""
    }
    val code              = s"use $typeCode$originalName$aliasCode"
    lazy val defaultAlias = originalName.split("\\\\").last

    val importNode = NewImport()
      .importedEntity(originalName)
      .importedAs(stmt.alias.map(_.name).getOrElse(defaultAlias))
      .isExplicit(true)
      .code(code)

    Ast(importNode)
  }

  private def astsForStaticStmt(stmt: PhpStaticStmt): List[Ast] = {
    stmt.vars.flatMap { staticVarDecl =>
      staticVarDecl.variable match {
        case PhpVariable(PhpNameExpr(name, _), _) =>
          val maybeDefaultValueAst = staticVarDecl.defaultValue.map(astForExpr)

          val code         = s"static $$$name"
          val typeFullName = maybeDefaultValueAst.flatMap(_.rootType).getOrElse(Defines.Any)

          val local =
            handleVariableOccurrence(stmt, name, Option(code), Option(typeFullName), List(ModifierTypes.STATIC))

          val assignmentAst = maybeDefaultValueAst.map { defaultValue =>
            val variableNode = identifierNode(stmt, name, s"$$$name", typeFullName)
            val variableAst  = Ast(variableNode).withRefEdge(variableNode, local)

            val assignCode = s"$code = ${defaultValue.rootCodeOrEmpty}"
            val assignNode = operatorCallNode(stmt, assignCode, Operators.assignment, None)

            callAst(assignNode, variableAst :: defaultValue :: Nil)
          }

          assignmentAst.toList

        case other =>
          logger.warn(s"Unexpected static variable type $other in $relativeFileName")
          Nil
      }
    }
  }

}

object AstCreator {
  object TypeConstants {
    val String: String              = "string"
    val Int: String                 = "int"
    val Float: String               = "float"
    val Bool: String                = "bool"
    val Void: String                = "void"
    val Array: String               = "array"
    val NullType: String            = "null"
    val VariadicPlaceholder: String = "PhpVariadicPlaceholder"
  }

  object NameConstants {
    val Default: String      = "default"
    val HaltCompiler: String = "__halt_compiler"
    val This: String         = "this"
    val Self: String         = "self"
    val Unknown: String      = "UNKNOWN"
    val Closure: String      = "__closure"
    val Class: String        = "class"
    val True: String         = "true"
    val False: String        = "false"
    val NullName: String     = "null"

    def isBoolean(name: String): Boolean = {
      List(True, False).contains(name)
    }

    def isNull(name: String): Boolean = {
      name.toLowerCase == NullName
    }
  }

  val operatorSymbols: Map[String, String] = Map(
    Operators.and                            -> "&",
    Operators.or                             -> "|",
    Operators.xor                            -> "^",
    Operators.logicalAnd                     -> "&&",
    Operators.logicalOr                      -> "||",
    PhpOperators.coalesceOp                  -> "??",
    PhpOperators.concatOp                    -> ".",
    Operators.division                       -> "/",
    Operators.equals                         -> "==",
    Operators.greaterEqualsThan              -> ">=",
    Operators.greaterThan                    -> ">",
    PhpOperators.identicalOp                 -> "===",
    PhpOperators.logicalXorOp                -> "xor",
    Operators.minus                          -> "-",
    Operators.modulo                         -> "%",
    Operators.multiplication                 -> "*",
    Operators.notEquals                      -> "!=",
    PhpOperators.notIdenticalOp              -> "!==",
    Operators.plus                           -> "+",
    Operators.exponentiation                 -> "**",
    Operators.shiftLeft                      -> "<<",
    Operators.arithmeticShiftRight           -> ">>",
    Operators.lessEqualsThan                 -> "<=",
    Operators.lessThan                       -> "<",
    PhpOperators.spaceshipOp                 -> "<=>",
    Operators.not                            -> "~",
    Operators.logicalNot                     -> "!",
    Operators.postDecrement                  -> "--",
    Operators.postIncrement                  -> "++",
    Operators.preDecrement                   -> "--",
    Operators.preIncrement                   -> "++",
    Operators.minus                          -> "-",
    Operators.plus                           -> "+",
    Operators.assignment                     -> "=",
    Operators.assignmentAnd                  -> "&=",
    Operators.assignmentOr                   -> "|=",
    Operators.assignmentXor                  -> "^=",
    PhpOperators.assignmentCoalesceOp        -> "??=",
    PhpOperators.assignmentConcatOp          -> ".=",
    Operators.assignmentDivision             -> "/=",
    Operators.assignmentMinus                -> "-=",
    Operators.assignmentModulo               -> "%=",
    Operators.assignmentMultiplication       -> "*=",
    Operators.assignmentPlus                 -> "+=",
    Operators.assignmentExponentiation       -> "**=",
    Operators.assignmentShiftLeft            -> "<<=",
    Operators.assignmentArithmeticShiftRight -> ">>="
  )

}
