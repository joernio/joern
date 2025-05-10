package io.joern.php2cpg.astcreation

import io.joern.php2cpg.astcreation.AstCreator.{NameConstants, TypeConstants}
import io.joern.php2cpg.parser.Domain.*
import io.joern.x2cpg.utils.AstPropertiesUtil.RootProperties
import io.joern.x2cpg.{Ast, Defines, ValidationMode}
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.codepropertygraph.generated.nodes.{NewImport, NewNamespaceBlock}

trait AstForDeclarationsCreator(implicit withSchemaValidation: ValidationMode) { this: AstCreator =>

  protected def astForDeclareStmt(stmt: PhpDeclareStmt): Ast = {
    val declareAssignAsts = stmt.declares.map(astForDeclareItem)
    val declareCode       = s"${PhpOperators.declareFunc}(${declareAssignAsts.map(_.rootCodeOrEmpty).mkString(",")})"
    val declareNode       = operatorCallNode(stmt, declareCode, PhpOperators.declareFunc, None)
    val declareAst        = callAst(declareNode, declareAssignAsts)

    stmt.stmts match {
      case Some(stmtList) =>
        val stmtAsts = stmtList.flatMap(astsForStmt)
        Ast(blockNode(stmt))
          .withChild(declareAst)
          .withChildren(stmtAsts)

      case None => declareAst
    }
  }

  private def astForDeclareItem(item: PhpDeclareItem): Ast = {
    val key   = identifierNode(item, item.key.name, item.key.name, Defines.Any)
    val value = astForExpr(item.value)
    val code  = s"${key.name}=${value.rootCodeOrEmpty}"

    val declareAssignment = operatorCallNode(item, code, Operators.assignment, None)
    callAst(declareAssignment, Ast(key) :: value :: Nil)
  }

  protected def astForGlobalStmt(stmt: PhpGlobalStmt): Ast = {
    // This isn't an accurater representation of what `global` does, but with things like `global $$x` being possible,
    // it's very difficult to figure out correct scopes for global variables.

    val varsAsts = stmt.vars.map(astForExpr)
    val code     = s"${PhpOperators.global} ${varsAsts.map(_.rootCodeOrEmpty).mkString(", ")}"

    val globalCallNode = operatorCallNode(stmt, code, PhpOperators.global, Some(TypeConstants.Void))

    callAst(globalCallNode, varsAsts)
  }

  protected def astForNamespaceStmt(stmt: PhpNamespaceStmt): Ast = {
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

  protected def astForUseStmt(stmt: PhpUseStmt): Ast = {
    // TODO Use useType + scope to get better name info
    val imports = stmt.uses.map(astForUseUse(_))
    wrapMultipleInBlock(imports, line(stmt))
  }

  protected def astForGroupUseStmt(stmt: PhpGroupUseStmt): Ast = {
    // TODO Use useType + scope to get better name info
    val groupPrefix = s"${stmt.prefix.name}\\"
    val imports     = stmt.uses.map(astForUseUse(_, groupPrefix))
    wrapMultipleInBlock(imports, line(stmt))
  }

  protected def astForTraitUseStmt(stmt: PhpTraitUseStmt): Ast = {
    // TODO Actually implement this
    logger.debug(
      s"Trait use statement encountered. This is not yet supported. Location: $relativeFileName:${line(stmt)}"
    )
    Ast(unknownNode(stmt, code(stmt)))
  }

  protected def astForUseUse(stmt: PhpUseUse, namePrefix: String = ""): Ast = {
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

  protected def astsForStaticStmt(stmt: PhpStaticStmt): List[Ast] = {
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
