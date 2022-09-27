/** originally from amm/compiler/src/main/scala-3/ammonite/compiler/AmmonitePhase.scala  */
package ammonite.foo

import ammonite.foo.{Name => AmmName}

import dotty.tools.dotc
import dotty.tools.dotc.core.StdNames.nme
import dotc.ast.Trees._
import dotc.ast.{tpd, untpd}
import dotc.core.Flags
import dotc.core.Contexts._
import dotc.core.Names.Name
import dotc.core.Phases.Phase
import dotc.core.Symbols.{NoSymbol, Symbol, newSymbol}
import dotc.core.Types.{TermRef, Type, TypeTraverser}

import scala.collection.mutable

class AmmonitePhase(
  userCodeNestingLevel: => Int,
  needsUsedEarlierDefinitions: => Boolean
) extends Phase:
  import tpd._

  def phaseName: String = "ammonite"

  private val myImports = new mutable.ListBuffer[(Boolean, String, String, Seq[AmmName])]
  private val usedEarlierDefinitions0 = new mutable.ListBuffer[String]

  def importData: Seq[ImportData] = {
    val grouped = myImports
      .toList
      .distinct
      .groupBy { case (a, b, c, d) => (b, c, d) }
      .view
      .mapValues(_.map(_._1))

    val open = for {
      ((fromName, toName, importString), items) <- grouped
      if !CompilerUtil.ignoredNames(fromName)
    } yield {
      val importType = items match {
        case Seq(true) => ImportData.Type
        case Seq(false) => ImportData.Term
        case Seq(_, _) => ImportData.TermType
        case _ => ???
      }

      ImportData(AmmName(fromName), AmmName(toName), importString, importType)
    }

    open.toVector.sortBy(x => Util.encodeScalaSourcePath(x.prefix))
  }

  def usedEarlierDefinitions: Seq[String] =
    usedEarlierDefinitions0.toList.distinct

  private def saneSym(name: Name, sym: Symbol)(using Context): Boolean =
    !name.decode.toString.contains('$') &&
    sym.exists &&
    // !sym.is(Flags.Synthetic) &&
    !scala.util.Try(sym.is(Flags.Private)).toOption.getOrElse(true) &&
    !scala.util.Try(sym.is(Flags.Protected)).toOption.getOrElse(true) &&
    // sym.is(Flags.Public) &&
    !CompilerUtil.ignoredSyms(sym.toString) &&
    !CompilerUtil.ignoredNames(name.decode.toString)

  private def saneSym(sym: Symbol)(using Context): Boolean =
    saneSym(sym.name, sym)

  private def processTree(t: tpd.Tree)(using Context): Unit = {
    val sym = t.symbol
    val name = t match {
      case t: tpd.ValDef => t.name
      case _ => sym.name
    }
    if (saneSym(name, sym)) {
      val name = sym.name.decode.toString
      myImports.addOne((sym.isType, name, name, Nil))
    }
  }

  private def processImport(i: tpd.Import)(using Context): Unit = {
    val expr = i.expr
    val selectors = i.selectors

    // Most of that logic was adapted from AmmonitePlugin, the Scala 2 counterpart
    // of this file.

    val prefix =
      val (_ :: nameListTail, symbolHead :: _) = {
        def rec(expr: tpd.Tree): List[(Name, Symbol)] = {
          expr match {
            case s @ tpd.Select(lhs, _) => (s.symbol.name -> s.symbol) :: rec(lhs)
            case i @ tpd.Ident(name) => List(name -> i.symbol)
            case t @ tpd.This(pkg) => List(pkg.name -> t.symbol)
          }
        }
        rec(expr).reverse.unzip
      }

      val headFullPath = symbolHead.fullName.decode.toString.split('.')
        .map(n => if (n.endsWith("$")) n.stripSuffix("$") else n) // meh
      // prefix package imports with `_root_` to try and stop random
      // variables from interfering with them. If someone defines a value
      // called `_root_`, this will still break, but that's their problem
      val rootPrefix = if(symbolHead.denot.is(Flags.Package)) Seq("_root_") else Nil
      val tailPath = nameListTail.map(_.decode.toString)

      (rootPrefix ++ headFullPath ++ tailPath).map(AmmName(_))

    def isMask(sel: untpd.ImportSelector) = sel.name != nme.WILDCARD && sel.rename == nme.WILDCARD

    val renameMap =

      /**
        * A map of each name importable from `expr`, to a `Seq[Boolean]`
        * containing a `true` if there's a type-symbol you can import, `false`
        * if there's a non-type symbol and both if there are both type and
        * non-type symbols that are importable for that name
        */
      val importableIsTypes =
        expr.tpe
            .allMembers
            .map(_.symbol)
            .filter(saneSym(_))
            .groupBy(_.name.decode.toString)
            .view
            .mapValues(_.map(_.isType).toVector)

      val renamings = for{
        t @ untpd.ImportSelector(name, renameTree, _) <- selectors
        if !isMask(t)
        // getOrElse just in case...
        isType <- importableIsTypes.getOrElse(name.name.decode.toString, Nil)
        Ident(rename) <- Option(renameTree)
      } yield ((isType, rename.decode.toString), name.name.decode.toString)

      renamings.toMap


    def isUnimportableUnlessRenamed(sym: Symbol): Boolean =
      sym eq NoSymbol

    @scala.annotation.tailrec
    def transformImport(selectors: List[untpd.ImportSelector], sym: Symbol): List[Symbol] =
      selectors match {
        case Nil => Nil
        case sel :: Nil if sel.isWildcard =>
          if (isUnimportableUnlessRenamed(sym)) Nil
          else List(sym)
        case (sel @ untpd.ImportSelector(from, to, _)) :: _
            if from.name == (if (from.isTerm) sym.name.toTermName else sym.name.toTypeName) =>
          if (isMask(sel)) Nil
          else List(
            newSymbol(sym.owner, sel.rename, sym.flags, sym.info, sym.privateWithin, sym.coord)
          )
        case _ :: rest => transformImport(rest, sym)
      }

    val symNames =
      for {
        sym <- expr.tpe.allMembers.map(_.symbol).flatMap(transformImport(selectors, _))
        if saneSym(sym)
      } yield (sym.isType, sym.name.decode.toString)

    val syms = for {
      // For some reason `info.allImportedSymbols` does not show imported
      // type aliases when they are imported directly e.g.
      //
      // import scala.reflect.macros.Context
      //
      // As opposed to via import scala.reflect.macros._.
      // Thus we need to combine allImportedSymbols with the renameMap
      (isType, sym) <- (symNames.toList ++ renameMap.keys).distinct
    } yield (isType, renameMap.getOrElse((isType, sym), sym), sym, prefix)

    myImports ++= syms
  }

  private def updateUsedEarlierDefinitions(
    wrapperSym: Symbol,
    stats: List[tpd.Tree]
  )(using Context): Unit = {
    /*
     * We list the variables from the first wrapper
     * used from the user code.
     *
     * E.g. if, after wrapping, the code looks like
     * ```
     *   class cmd2 {
     *
     *     val cmd0 = ???
     *     val cmd1 = ???
     *
     *     import cmd0.{
     *       n
     *     }
     *
     *     class Helper {
     *       // user-typed code
     *       val n0 = n + 1
     *     }
     *   }
     * ```
     * this would process the tree of `val n0 = n + 1`, find `n` as a tree like
     * `cmd2.this.cmd0.n`, and put `cmd0` in `uses`.
     */

    val typeTraverser: TypeTraverser = new TypeTraverser {
      def traverse(tpe: Type) = tpe match {
        case tr: TermRef if tr.prefix.typeSymbol == wrapperSym =>
          tr.designator match {
            case n: Name => usedEarlierDefinitions0 += n.decode.toString
            case s: Symbol => usedEarlierDefinitions0 += s.name.decode.toString
            case _ => // can this happen?
          }
        case _ =>
          traverseChildren(tpe)
      }
    }

    val traverser: TreeTraverser = new TreeTraverser {
      def traverse(tree: Tree)(using Context) = tree match {
        case tpd.Select(node, name) if node.symbol == wrapperSym =>
          usedEarlierDefinitions0 += name.decode.toString
        case tt @ tpd.TypeTree() =>
          typeTraverser.traverse(tt.tpe)
        case _ =>
          traverseChildren(tree)
      }
    }

    for (tree <- stats)
      traverser.traverse(tree)
  }

  private def unpkg(tree: tpd.Tree): List[tpd.Tree] =
    tree match {
      case PackageDef(_, elems) => elems.flatMap(unpkg)
      case _ => List(tree)
    }

  def run(using Context): Unit = {
    val elems = unpkg(ctx.compilationUnit.tpdTree)

    // this code extracts the user input from ammonites nested generated trees...
    // we try and get away without such nesting, therefor not needed for now...
    // TODO drop in final cleanup
//    def mainStats(trees: List[tpd.Tree]): List[tpd.Tree] =
//      trees
//        .reverseIterator
//        .collectFirst {
//          case TypeDef(name, rhs0: Template) => rhs0.body
//        }
//        .getOrElse(Nil)
//
//    val rootStats = mainStats(elems)
//    val stats = (1 until userCodeNestingLevel)
//      .foldLeft(rootStats)((trees, _) => mainStats(trees))
//
//    if (needsUsedEarlierDefinitions) {
//      val wrapperSym = elems.last.symbol
//      updateUsedEarlierDefinitions(wrapperSym, stats)
//    }
    val stats = elems

    stats.foreach {
      case i: Import => processImport(i)
      case t: tpd.DefDef => processTree(t)
      case t: tpd.ValDef => processTree(t)
      case t: tpd.TypeDef =>
        processTree(t)
      case _ =>
    }
  }
