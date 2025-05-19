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
    val fullName                   = MetaDataPass.getGlobalNamespaceBlockFullName(None)
    val stack: NamespaceScopeStack = Seq(fullName)

    val children = result.children.flatMap {
      case namespace: PhpNamespaceStmt if namespace.name.isEmpty => namespace.stmts.flatMap(visit(_, stack, None))
      case stmt                                                  => visit(stmt, stack, None).toList
    }.dedup
    summary.add(globalNamespace(children))
  }

  override def finish(): Unit = {
    val dedupSummary = summary.asScala.toSeq.dedup
    captureSummary(dedupSummary)
  }

  private def visit(
    node: PhpNode,
    stack: NamespaceScopeStack,
    parent: Option[SymbolSummary & HasChildren]
  ): Seq[SymbolSummary] = {
    node match {
      case stmt: PhpNamespaceStmt                                  => visitNamespaceStmt(stmt, stack, parent)
      case method: PhpMethodDecl                                   => visitMethodDecl(method, parent)
      case classLike: PhpClassLikeStmt if classLike.name.isDefined => visitClassDecl(classLike, stack, parent)
      case cs: PhpConstStmt                                        => cs.consts.map(c => PhpMember(c.name.name, parent))
      case cp: PhpPropertyStmt => cp.variables.map(c => PhpMember(c.name.name, parent))
      case _                   => Nil
    }
  }

  private def visitNamespaceStmt(
    stmt: PhpNamespaceStmt,
    stack: NamespaceScopeStack,
    parent: Option[SymbolSummary & HasChildren]
  ): Seq[SymbolSummary] = {
    stmt.name match {
      case None                                => stmt.stmts.flatMap(visit(_, stack, parent)).dedup
      case Some(fullName) if stack.sizeIs == 1 =>
        // We are the first namespace declaration in a possibly nested namespace, so this name should be fully separated
        val nameParts               = fullName.name.split("\\\\")
        val name                    = nameParts.last
        val namespaceScopeToPrepend = (nameParts.reverse ++ stack).toSeq
        val children                = stmt.stmts.flatMap(stmt => visit(stmt, namespaceScopeToPrepend, None)).dedup
        nameParts.reverse.tail.foldLeft(PhpNamespace(name, name, children, parent))((acc, name) =>
          PhpNamespace(name, name, acc :: Nil, None) // This will be re-wired by the caller to be its parent
        ) :: Nil
      case Some(fullName) =>
        // We are not the first namespace in a possibly nested namespace, thus we should only use the last part
        val nameParts = fullName.name.split("\\\\")
        val name      = nameParts.last
        val children  = stmt.stmts.flatMap(stmt => visit(stmt, nameParts.last +: stack, None)).dedup
        PhpNamespace(name, name, children, parent) :: Nil
    }
  }

  private def visitMethodDecl(
    method: PhpMethodDecl,
    parent: Option[SymbolSummary & HasChildren]
  ): Seq[SymbolSummary] = {
    val name = method.name.name
    PhpFunction(name, parent) :: Nil
  }

  private def visitClassDecl(
    classLike: PhpClassLikeStmt,
    stack: NamespaceScopeStack,
    parent: Option[SymbolSummary & HasChildren]
  ): Seq[SymbolSummary] = {
    val name        = classLike.name.map(_.name).get
    val constructor = if !classLike.hasConstructor then Nil else PhpFunction(Domain.ConstructorMethodName, None) :: Nil
    val children    = constructor ++ classLike.stmts.flatMap(visit(_, stack, None))
    PhpClass(name, children, parent) :: Nil // This will re-wire children to link to PhpClass
  }

}

object SymbolSummaryPass {

  private type NamespaceScopeStack = Seq[String]

  def globalNamespace(children: Seq[SymbolSummary] = Nil): PhpNamespace = {
    val name = MetaDataPass.getGlobalNamespaceBlockFullName(None)
    PhpNamespace(name, name, children, None)
  }

  private implicit class SummarySeqExt(xs: Seq[SymbolSummary]) {

    /** Deduplicates a sequence of summaries at a specific namespace level. Particularly useful for namespaces declared
      * adjacent to one another.
      */
    def dedup: Seq[SymbolSummary] = {
      xs.foldLeft(Seq.empty[SymbolSummary])((acc, next) =>
        acc match {
          case Nil          => next :: Nil
          case head :: tail => head + next ++ tail
        }
      )
    }

  }

  sealed trait SymbolSummary {

    /** The link back to the parent object.
      * @return
      *   the parent symbol summary.
      */
    def _parent: Option[SymbolSummary & HasChildren]

    /** Follows up the tree to the parent, excluding other siblings along the way.
      * @return
      *   the global-level symbol summary, containing a path to this symbol summary the only leaf.
      */
    def sparseTree: SymbolSummary = {
      _parent match {
        case None    => this
        case Some(p) => p.withChildren(this :: Nil).sparseTree
      }
    }

    /** Combines two symbol summary objects at the same namespace/AST level.
      * @param o
      *   the object to combine with.
      * @return
      *   two separate objects if they cannot be combined, or a single combined object if otherwise.
      */
    def +(o: SymbolSummary): Seq[SymbolSummary]

    /** @return
      *   the identifier of the symbol as per its definition.
      */
    def name: String

    /** @return
      *   the identifier of the symbol as defined by the importing script.
      */
    def alias: String

    /** @param parent
      *   the parent to link this child to.
      * @return
      *   a new copy of this node, with the parent field linked to the given parent.
      */
    def _withParent(parent: SymbolSummary & HasChildren): SymbolSummary

  }

  sealed trait HasChildren { this: SymbolSummary =>

    def children: Seq[SymbolSummary]

    def withChildren(newChildren: Seq[SymbolSummary]): SymbolSummary & HasChildren

    protected def combineChildren(o: SymbolSummary & HasChildren): Seq[SymbolSummary] = {
      (this.children ++ o.children).foldLeft(Seq.empty[SymbolSummary])((acc, next) =>
        acc match {
          case Nil          => next :: Nil
          case head :: tail => head + next ++ tail
        }
      )
    }

  }

  case class PhpNamespace(
    name: String,
    alias: String,
    children: Seq[SymbolSummary],
    _parent: Option[SymbolSummary & HasChildren]
  ) extends SymbolSummary
      with HasChildren {
    override def +(o: SymbolSummary): Seq[SymbolSummary] = {
      o match {
        case n: PhpNamespace if n.name == this.name =>
          new PhpNamespace(name, alias, combineChildren(n), this._parent) :: Nil
        case _ => this :: o :: Nil
      }
    }

    override def withChildren(newChildren: Seq[SymbolSummary]): SymbolSummary & HasChildren = {
      this.copy(children = newChildren)
    }

    override def _withParent(parent: SymbolSummary & HasChildren): SymbolSummary = this.copy(_parent = Option(parent))
  }

  /** Constructs a PhpNamespace object with a parent pointer pointing to its parent attached to the children nodes.
    * Using immutable structures in this way means we should not traverse this tree top-down.
    */
  object PhpNamespace {

    def apply(name: String, children: Seq[SymbolSummary]): PhpNamespace = PhpNamespace(name, name, children, None)

    def apply(
      name: String,
      alias: String,
      children: Seq[SymbolSummary],
      maybeParent: Option[SymbolSummary & HasChildren] = None
    ): PhpNamespace = {
      val self = maybeParent match {
        case None => new PhpNamespace(name, alias, Nil, Option(globalNamespace()))
        case Some(parent) => new PhpNamespace(name, alias, Nil, Option(parent))
      }
      self.copy(children = children.map(_._withParent(self)))
    }

    def unapply(x: PhpNamespace): (String, Seq[SymbolSummary]) =
      (x.name, x.children)
  }

  case class PhpFunction(name: String, alias: String, _parent: Option[SymbolSummary & HasChildren])
      extends SymbolSummary {
    override def +(o: SymbolSummary): Seq[SymbolSummary] = {
      o match {
        case f: PhpFunction if f.name == this.name => this :: Nil
        case _                                     => this :: o :: Nil
      }
    }

    override def _withParent(parent: SymbolSummary & HasChildren): SymbolSummary = this.copy(_parent = Option(parent))

  }

  object PhpFunction {

    def apply(name: String): PhpFunction = PhpFunction(name, name)

    def apply(name: String, maybeParent: Option[SymbolSummary & HasChildren]): PhpFunction =
      PhpFunction(name, name, maybeParent)

    def apply(name: String, alias: String): PhpFunction = new PhpFunction(name, alias, Option(globalNamespace()))

    def unapply(x: PhpFunction): Option[String] = Option(x.name)
  }

  case class PhpClass(
    name: String,
    alias: String,
    children: Seq[SymbolSummary],
    _parent: Option[SymbolSummary & HasChildren]
  ) extends SymbolSummary
      with HasChildren {
    override def +(o: SymbolSummary): Seq[SymbolSummary] = {
      o match {
        case c: PhpClass if c.name == this.name =>
          new PhpClass(name, alias, combineChildren(c), this._parent) :: Nil
        case _ => this :: o :: Nil
      }
    }

    override def withChildren(newChildren: Seq[SymbolSummary]): SymbolSummary & HasChildren =
      this.copy(children = newChildren)

    override def _withParent(parent: SymbolSummary & HasChildren): SymbolSummary = this.copy(_parent = Option(parent))
  }

  object PhpClass {

    def apply(name: String, children: Seq[SymbolSummary], maybeParent: Option[SymbolSummary & HasChildren]): PhpClass =
      PhpClass(name, name, children, maybeParent)

    /** Constructs a PhpClass object with a parent pointer pointing to its parent attached to the children nodes. Using
      * immutable structures in this way means we should not traverse this tree top-down.
      */
    def apply(
      name: String,
      alias: String,
      children: Seq[SymbolSummary],
      maybeParent: Option[SymbolSummary & HasChildren] = None
    ): PhpClass = {
      maybeParent match {
        case None => new PhpClass(name, alias, children, Option(globalNamespace()))
        case Some(parent) =>
          val self = new PhpClass(name, alias, Nil, Option(parent))
          self.copy(children = children.map(_._withParent(self)))
      }
    }

    def unapply(x: PhpClass): (String, Seq[SymbolSummary]) =
      (x.name, x.children)
  }

  case class PhpMember(name: String, alias: String, _parent: Option[SymbolSummary & HasChildren])
      extends SymbolSummary {
    override def +(o: SymbolSummary): Seq[SymbolSummary] = {
      o match {
        case f: PhpMember if f.name == this.name => this :: Nil
        case _                                   => this :: o :: Nil
      }
    }

    override def _withParent(parent: SymbolSummary & HasChildren): SymbolSummary = this.copy(_parent = Option(parent))

  }

  object PhpMember {
    def apply(name: String, maybeParent: Option[SymbolSummary & HasChildren]): PhpMember =
      new PhpMember(name, name, maybeParent)

    def apply(name: String): PhpMember = new PhpMember(name, name, Option(globalNamespace()))

    def unapply(x: PhpMember): Option[String] = Option(x.name)
  }

  case class PhpExternal(name: String, alias: String, _parent: Option[SymbolSummary & HasChildren])
      extends SymbolSummary {
    override def +(o: SymbolSummary): Seq[SymbolSummary] = {
      o match {
        case f: PhpExternal if f.name == this.name => this :: Nil
        case _                                     => this :: o :: Nil
      }
    }

    override def _withParent(parent: SymbolSummary & HasChildren): SymbolSummary = this.copy(_parent = Option(parent))
  }

  object PhpExternal {
    def apply(name: String): PhpExternal = PhpExternal(name, name)

    def apply(name: String, alias: String): PhpExternal = new PhpExternal(name, alias, Option(globalNamespace()))

    def unapply(x: PhpExternal): String = x.name
  }

}
