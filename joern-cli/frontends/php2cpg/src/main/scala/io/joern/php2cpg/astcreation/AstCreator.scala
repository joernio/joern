package io.joern.php2cpg.astcreation

import io.joern.php2cpg.astcreation.AstCreator.{NameConstants, TypeConstants, operatorSymbols}
import io.joern.php2cpg.datastructures.ArrayIndexTracker
import io.joern.php2cpg.parser.Domain.*
import io.joern.php2cpg.parser.Domain.PhpModifiers.containsAccessModifier
import io.joern.php2cpg.utils.Scope
import io.joern.x2cpg.Ast.storeInDiffGraph
import io.joern.x2cpg.Defines.{StaticInitMethodName, UnresolvedNamespace, UnresolvedSignature}
import io.joern.x2cpg.utils.AstPropertiesUtil.RootProperties
import io.joern.x2cpg.utils.IntervalKeyPool
import io.joern.x2cpg.{Ast, AstCreatorBase, AstNodeBuilder, Defines, ValidationMode}
import io.shiftleft.codepropertygraph.generated.*
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.types.structure.NamespaceTraversal
import io.shiftleft.utils.IOUtils
import org.slf4j.{Logger, LoggerFactory}

import java.nio.charset.StandardCharsets
import java.nio.file.Path
import scala.collection.mutable

class AstCreator(
  protected val relativeFileName: String,
  fileName: String,
  phpAst: PhpFile,
  disableFileContent: Boolean
)(implicit withSchemaValidation: ValidationMode)
    extends AstCreatorBase[PhpNode, AstCreator](relativeFileName)
    with AstCreatorHelper(disableFileContent)
    with AstForExpressionsCreator
    with AstForControlStructuresCreator
    with AstForDeclarationsCreator
    with AstForFunctionsCreator
    with AstForTypesCreator {

  protected val logger: Logger = LoggerFactory.getLogger(AstCreator.getClass)
  protected val scope          = new Scope()(() => nextClosureName())
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
    PhpMethodDecl(
      name = PhpNameExpr(NamespaceTraversal.globalNamespaceName, file.attributes),
      params = Nil,
      modifiers = modifiersList,
      returnType = None,
      stmts = bodyStmts,
      returnByRef = false,
      namespacedName = None,
      isClassMethod = false,
      attributes = file.attributes,
      attributeGroups = Seq.empty[PhpAttributeGroup]
    )
  }

  private def astForPhpFile(file: PhpFile): Ast = {
    val fileNode = NewFile().name(relativeFileName)
    fileContent.foreach(fileNode.content(_))

    scope.pushNewScope(globalNamespace)

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

    val globalTypeDeclAst = astForClassLikeStmt(globalTypeDeclStmt)

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
      case classLikeStmt: PhpClassLikeStmt => astForClassLikeStmt(classLikeStmt) :: Nil
      case gotoStmt: PhpGotoStmt           => astForGotoStmt(gotoStmt) :: Nil
      case labelStmt: PhpLabelStmt         => astForLabelStmt(labelStmt) :: Nil
      case namespace: PhpNamespaceStmt     => astForNamespaceStmt(namespace) :: Nil
      case declareStmt: PhpDeclareStmt     => astForDeclareStmt(declareStmt) :: Nil
      case _: NopStmt                      => Nil // TODO This'll need to be updated when comments are added.
      case haltStmt: PhpHaltCompilerStmt   => astForHaltCompilerStmt(haltStmt) :: Nil
      case unsetStmt: PhpUnsetStmt         => astForUnsetStmt(unsetStmt) :: Nil
      case globalStmt: PhpGlobalStmt       => astForGlobalStmt(globalStmt) :: Nil
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

    scope.pushNewScope(namespaceBlock)
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

  private def astForGlobalStmt(stmt: PhpGlobalStmt): Ast = {
    // This isn't an accurater representation of what `global` does, but with things like `global $$x` being possible,
    // it's very difficult to figure out correct scopes for global variables.

    val varsAsts = stmt.vars.map(astForExpr)
    val code     = s"${PhpOperators.global} ${varsAsts.map(_.rootCodeOrEmpty).mkString(", ")}"

    val globalCallNode = operatorCallNode(stmt, code, PhpOperators.global, Some(TypeConstants.Void))

    callAst(globalCallNode, varsAsts)
  }

  private def astForUseStmt(stmt: PhpUseStmt): Ast = {
    // TODO Use useType + scope to get better name info
    val imports = stmt.uses.map(astForUseUse(_))
    wrapMultipleInBlock(imports, line(stmt))
  }

  private def astForGroupUseStmt(stmt: PhpGroupUseStmt): Ast = {
    // TODO Use useType + scope to get better name info
    val groupPrefix = s"${stmt.prefix.name}\\"
    val imports     = stmt.uses.map(astForUseUse(_, groupPrefix))
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
    val code = s"use $typeCode$originalName$aliasCode"

    val importNode = NewImport()
      .importedEntity(originalName)
      .importedAs(stmt.alias.map(_.name))
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

          val local = localNode(stmt, name, code, typeFullName)
          scope.addToScope(local.name, local)

          val assignmentAst = maybeDefaultValueAst.map { defaultValue =>
            val variableNode = identifierNode(stmt, name, s"$$$name", typeFullName)
            val variableAst  = Ast(variableNode).withRefEdge(variableNode, local)

            val assignCode = s"$code = ${defaultValue.rootCodeOrEmpty}"
            val assignNode = operatorCallNode(stmt, assignCode, Operators.assignment, None)

            callAst(assignNode, variableAst :: defaultValue :: Nil)
          }

          Ast(local) :: assignmentAst.toList

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
