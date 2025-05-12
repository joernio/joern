package io.joern.php2cpg.passes

import io.joern.php2cpg.Config
import io.joern.php2cpg.parser.Domain.*
import io.joern.php2cpg.parser.{Domain, PhpParser}
import io.joern.php2cpg.passes.SymbolSummaryPass.{PhpNamespace, SymbolSummary}
import io.joern.x2cpg.passes.frontend.MetaDataPass
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.passes.ForkJoinParallelCpgPass
import scala.jdk.CollectionConverters.*

/** Gathers all the symbols from types and methods one can import from each PHP script and namespace.
  *
  * @param captureSummary
  *   when the pass has finished summarizing all files, this method captures the result. This is thread safe.
  */
class SymbolSummaryPass(config: Config, cpg: Cpg, parser: PhpParser, captureSummary: Seq[SymbolSummary] => Unit)
    extends ForkJoinParallelCpgPass[BatchOfPhpScripts](cpg)
    with AstParsingPass(config, parser) {

  private val summary = new java.util.concurrent.ConcurrentLinkedQueue[SymbolSummary]()

  import io.joern.php2cpg.passes.SymbolSummaryPass.*

  override def processPart(builder: DiffGraphBuilder, fileName: String, result: Domain.PhpFile): Unit = {
    val fullName = MetaDataPass.getGlobalNamespaceBlockFullName(None)
    val children = result.children.flatMap {
      case namespace: PhpNamespaceStmt if namespace.name.isEmpty => namespace.stmts.flatMap(visit)
      case stmt                                                  => visit(stmt).toList
    }
    summary.add(PhpNamespace(fullName, children))
  }

  override def finish(): Unit = {
    val dedupSummary = SymbolSummaryPass.deduplicateSummary(summary.asScala.toSeq)
    captureSummary(dedupSummary)
  }

  private def visit(node: PhpNode): Seq[SymbolSummary] = {
    node match {
      case stmt: PhpNamespaceStmt                                  => visitNamespaceStmt(stmt)
      case method: PhpMethodDecl                                   => visitMethodDecl(method)
      case classLike: PhpClassLikeStmt if classLike.name.isDefined => visitClassDecl(classLike)
      case cs: PhpConstStmt                                        => cs.consts.map(c => PhpMember(c.name.name))
      case cp: PhpPropertyStmt                                     => cp.variables.map(c => PhpMember(c.name.name))
      case _                                                       => Nil
    }
  }

  private def visitNamespaceStmt(stmt: PhpNamespaceStmt): Seq[SymbolSummary] = {
    val children = stmt.stmts.flatMap(visit)
    stmt.name match {
      case None => children
      case Some(name) =>
        val nameParts = name.name.split("\\\\")
        nameParts.tail.foldLeft(PhpNamespace(nameParts.head, children))((acc, name) =>
          val nextNamespace = PhpNamespace(name, acc.children)
          acc.copy(children = nextNamespace :: Nil)
        ) :: Nil
    }
  }

  private def visitMethodDecl(method: PhpMethodDecl): Seq[SymbolSummary] = {
    val name = method.name.name
    PhpFunction(name) :: Nil
  }

  private def visitClassDecl(classLike: PhpClassLikeStmt): Seq[SymbolSummary] = {
    val name        = classLike.name.map(_.name).get
    val constructor = if !classLike.hasConstructor then Nil else PhpFunction(Domain.ConstructorMethodName) :: Nil
    val children    = constructor ++ classLike.stmts.flatMap(visit)
    PhpClass(name, children) :: Nil
  }

}

object SymbolSummaryPass {

  def deduplicateSummary(summary: Seq[SymbolSummary]): Seq[SymbolSummary] = {
    summary.foldLeft(Seq.empty[SymbolSummary])((acc, next) =>
      acc match {
        case Nil          => next :: Nil
        case head :: tail => head + next ++ tail
      }
    )
  }

  sealed trait SymbolSummary {

    /** Combines two symbol summary objects at the same namespace/AST level.
      * @param o
      *   the object to combine with.
      * @return
      *   two separate objects if they cannot be combined, or a single combined object if otherwise.
      */
    def +(o: SymbolSummary): Seq[SymbolSummary]
  }

  sealed trait HasChildren { this: SymbolSummary =>

    def children: Seq[SymbolSummary]

    protected def combineChildren(o: SymbolSummary & HasChildren): Seq[SymbolSummary] = {
      (this.children ++ o.children).foldLeft(Seq.empty[SymbolSummary])((acc, next) =>
        acc match {
          case Nil          => next :: Nil
          case head :: tail => head + next ++ tail
        }
      )
    }

  }

  case class PhpNamespace(name: String, children: Seq[SymbolSummary]) extends SymbolSummary with HasChildren {
    override def +(o: SymbolSummary): Seq[SymbolSummary] = {
      o match {
        case n: PhpNamespace if n.name == this.name =>
          PhpNamespace(name, combineChildren(n)) :: Nil
        case _ => this :: o :: Nil
      }
    }
  }

  case class PhpFunction(name: String) extends SymbolSummary {
    override def +(o: SymbolSummary): Seq[SymbolSummary] = {
      o match {
        case f: PhpFunction if f.name == this.name => this :: Nil
        case _                                     => this :: o :: Nil
      }
    }
  }

  case class PhpClass(name: String, children: Seq[SymbolSummary]) extends SymbolSummary with HasChildren {
    override def +(o: SymbolSummary): Seq[SymbolSummary] = {
      o match {
        case c: PhpClass if c.name == this.name =>
          PhpClass(name, combineChildren(c)) :: Nil
        case _ => this :: o :: Nil
      }
    }
  }

  case class PhpMember(name: String) extends SymbolSummary {
    override def +(o: SymbolSummary): Seq[SymbolSummary] = {
      o match {
        case f: PhpMember if f.name == this.name => this :: Nil
        case _                                   => this :: o :: Nil
      }
    }
  }

}
