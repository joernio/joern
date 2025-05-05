package io.joern.swiftsrc2cpg.astcreation

import io.joern.swiftsrc2cpg.parser.SwiftNodeSyntax.AccessorDeclSyntax
import io.joern.swiftsrc2cpg.parser.SwiftNodeSyntax.CodeBlockItemSyntax
import io.joern.swiftsrc2cpg.parser.SwiftNodeSyntax.DeferStmtSyntax
import io.joern.swiftsrc2cpg.parser.SwiftNodeSyntax.DeinitializerDeclSyntax
import io.joern.swiftsrc2cpg.parser.SwiftNodeSyntax.FunctionDeclSyntax
import io.joern.swiftsrc2cpg.parser.SwiftNodeSyntax.GuardStmtSyntax
import io.joern.swiftsrc2cpg.parser.SwiftNodeSyntax.InitializerDeclSyntax
import io.joern.swiftsrc2cpg.parser.SwiftNodeSyntax.SwiftNode
import io.joern.x2cpg.Ast
import io.joern.x2cpg.ValidationMode
import io.joern.x2cpg.utils.IntervalKeyPool
import io.shiftleft.codepropertygraph.generated.nodes.NewNode
import io.shiftleft.codepropertygraph.generated.ControlStructureTypes
import io.shiftleft.codepropertygraph.generated.PropertyNames

import scala.collection.mutable

object AstCreatorHelper {

  implicit class OptionSafeAst(val ast: Ast) extends AnyVal {
    def withArgEdge(src: NewNode, dst: Option[NewNode]): Ast = dst match {
      case Some(value) => ast.withArgEdge(src, value)
      case None        => ast
    }
  }

}

trait AstCreatorHelper(implicit withSchemaValidation: ValidationMode) { this: AstCreator =>

  private val fileLocalNameKeyPool = new IntervalKeyPool(first = 0, last = Long.MaxValue)

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
      val childrenAsts = otherElements.map(astForNode) ++ deferElementsAstsOrdered
      setArgumentIndices(childrenAsts)
      childrenAsts
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
          setArgumentIndices(blockChildren)
          blockAst(block, blockChildren)
      }
      val elseAst = astForNode(guardStmt.body)
      setOrderExplicitly(elseAst, 3)

      val ifAst         = controlStructureAst(ifNode, Option(conditionAst), Seq(thenAst, elseAst))
      val resultingAsts = astsForBlockElements(elementsBeforeGuard) :+ ifAst
      setArgumentIndices(resultingAsts)
      resultingAsts
    }
  }

  protected def astParentInfo(): (String, String) = {
    val astParentType     = methodAstParentStack.head.label
    val astParentFullName = methodAstParentStack.head.properties(PropertyNames.FULL_NAME).toString
    (astParentType, astParentFullName)
  }

  protected def registerType(typeFullName: String): Unit = {
    global.usedTypes.putIfAbsent(typeFullName, true)
  }

  protected def calcTypeNameAndFullName(name: String): (String, String) = {
    val fullNamePrefix = s"${parserResult.filename}:${scope.computeScopePath}:"
    val fullName       = s"$fullNamePrefix$name"
    (name, fullName)
  }

  protected def fileLocalUniqueName(name: String, fullName: String, targetName: String = ""): (String, String) = {
    if (name.isEmpty && (fullName.isEmpty || fullName.endsWith("."))) {
      val newName = targetName match {
        case ""    => s"<anonymous>${fileLocalNameKeyPool.next}"
        case other => s"<$other>${fileLocalNameKeyPool.next}"
      }
      val resultingFullName = s"$fullName$newName"
      (newName, resultingFullName)
    } else {
      (name, fullName)
    }
  }

  protected def calcMethodNameAndFullName(func: SwiftNode): (String, String) = {
    val name           = calcMethodName(func)
    val fullNamePrefix = s"${parserResult.filename}:${scope.computeScopePath}:"
    val fullName       = s"$fullNamePrefix$name"
    (name, fullName)
  }

  private def calcMethodName(func: SwiftNode): String = func match {
    case f: FunctionDeclSyntax      => code(f.name)
    case a: AccessorDeclSyntax      => code(a.accessorSpecifier)
    case d: DeinitializerDeclSyntax => code(d.deinitKeyword)
    case i: InitializerDeclSyntax   => code(i.initKeyword)
    case _                          => nextClosureName()
  }

}
