package io.joern.php2cpg.passes

import io.joern.php2cpg.Config
import io.joern.php2cpg.parser.Domain.*
import io.joern.php2cpg.parser.{Domain, PhpParser}
import io.joern.php2cpg.passes.SymbolSummaryPass.{PhpNamespace, SymbolSummary}
import io.joern.x2cpg.passes.frontend.MetaDataPass
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.passes.ForkJoinParallelCpgPass
import scala.jdk.CollectionConverters.*

/** Gathers all the symbols from namespaces, types, and methods one can import from each PHP script and namespace. Class
  * constants cannot be imported directly, thus we do not handle them.
  *
  * @param captureSummary
  *   when the pass has finished summarizing all files, this method captures the result. This is thread safe.
  */
class SymbolSummaryPass(
  config: Config,
  cpg: Cpg,
  parser: PhpParser,
  captureSummary: Map[String, Seq[SymbolSummary]] => Unit
) extends ForkJoinParallelCpgPass[BatchOfPhpScripts](cpg)
    with AstParsingPass(config, parser) {

  private val summary = new java.util.concurrent.ConcurrentLinkedQueue[SymbolSummary]()

  import io.joern.php2cpg.passes.SymbolSummaryPass.*

  override def processPart(builder: DiffGraphBuilder, fileName: String, result: Domain.PhpFile): Unit = {
    val stack: NamespaceScopeStack = Seq.empty

    val summaries = result.children.flatMap {
      case namespace: PhpNamespaceStmt if namespace.name.isEmpty => namespace.stmts.flatMap(visit(_, stack))
      case stmt                                                  => visit(stmt, stack).toList
    }.distinct
    summary.addAll(summaries.asJava)
  }

  override def finish(): Unit = {
    val summaryMap = summary.asScala.toSeq.distinct.groupBy(_.name)
    captureSummary(summaryMap)
  }

  private def visit(node: PhpNode, stack: NamespaceScopeStack): Seq[SymbolSummary] = {
    node match {
      case stmt: PhpNamespaceStmt                                  => visitNamespaceStmt(stmt, stack)
      case method: PhpMethodDecl                                   => visitMethodDecl(method, stack)
      case classLike: PhpClassLikeStmt if classLike.name.isDefined => visitClassDecl(classLike, stack)
      case _                                                       => Nil
    }
  }

  private def visitNamespaceStmt(stmt: PhpNamespaceStmt, stack: NamespaceScopeStack): Seq[SymbolSummary] = {
    stmt.name.map(_.name) match {
      case None                            => stmt.stmts.flatMap(visit(_, stack))
      case Some(fullName) if stack.isEmpty =>
        // We are the first namespace declaration in a possibly nested namespace, so this name should be fully separated
        val nameParts = fullName.split("\\\\")
        val name      = nameParts.head
        val newStack  = (nameParts.reverse ++ stack).toSeq
        val children  = stmt.stmts.flatMap(stmt => visit(stmt, newStack)).distinct
        val namespaces = nameParts.tail.foldRight(PhpNamespace(name) :: Nil)((name, acc) => {
          val newName = acc.lastOption.map(_.name).toList :+ name mkString "\\"
          PhpNamespace(newName) :: acc
        })
        namespaces ++ children
      case Some(fullName) =>
        // We are not the first namespace in a possibly nested namespace, thus we should only use the last part
        val nameParts = fullName.split("\\\\")
        val children  = stmt.stmts.flatMap(stmt => visit(stmt, nameParts.last +: stack)).distinct
        PhpNamespace(fullName) :: children
    }
  }

  private def visitMethodDecl(method: PhpMethodDecl, stack: NamespaceScopeStack): Seq[SymbolSummary] = {
    val name = method.name.fullName(stack)
    PhpFunction(name) :: Nil
  }

  private def visitClassDecl(classLike: PhpClassLikeStmt, stack: NamespaceScopeStack): Seq[SymbolSummary] =
    classLike.name match {
      case None => Nil
      case Some(nameExpr) =>
        val classFullName = nameExpr.fullName(stack)
        PhpClass(classFullName) :: Nil // children are ignored, as they cannot be imported directly
    }

}

object SymbolSummaryPass {

  private type NamespaceScopeStack = Seq[String]

  private implicit class NameExprExt(nameExpr: PhpNameExpr) {

    def fullName(stack: NamespaceScopeStack): String = stack.reverse :+ nameExpr.name mkString "\\"

  }

  sealed trait SymbolSummary extends Ordered[SymbolSummary] {

    /** @return
      *   the fully qualified identifier of the symbol as per its definition.
      */
    def name: String

    /** Allows symbols to be compared. When resolving imports, the order of precedence is: Classes, Functions,
      * Constants, Namespaces.
      * @param that
      *   the other symbol.
      * @return
      *   the outcome of the comparison.
      */
    def compare(that: SymbolSummary): Int = {
      val typeOrder = (this, that) match {
        case (_: PhpNamespace, _: PhpFunction) => 1
        case (_: PhpNamespace, _: PhpClass)    => 1
        case (_: PhpFunction, _: PhpClass)     => 1
        case (_: PhpFunction, _: PhpNamespace) => -1
        case (_: PhpClass, _: PhpNamespace)    => -1
        case (_: PhpClass, _: PhpFunction)     => -1
        case _                                 => 0
      }

      if (typeOrder != 0) typeOrder
      else this.name.compare(that.name)
    }

  }

  case class PhpNamespace(name: String) extends SymbolSummary

  case class PhpFunction(name: String) extends SymbolSummary

  case class PhpClass(name: String) extends SymbolSummary

}
