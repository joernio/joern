package fix

import scalafix.v1._
import scala.collection.concurrent.TrieMap
import scala.collection.mutable
import scala.meta._

object UnorderedIteration {

  // Deliberately excludes insertion- or sort-ordered types (LinkedHashMap, TreeMap, VectorMap,
  // SeqMap, ...): their encounter order is well-defined. Included conservatively despite being
  // deterministic for fixed contents in practice: immutable.HashMap/HashSet (not guaranteed
  // across platforms/compiler versions or with identity hashing), immutable.IntMap/LongMap
  // (unsigned-trie order — neither sorted nor insertion order) and priority queues (heap
  // order; their contracts state the iterator traverses in no particular order).
  private val unorderedTypes: Seq[String] = Seq(
    "scala/collection/mutable/HashMap#",
    "scala/collection/mutable/HashSet#",
    "scala/collection/mutable/AnyRefMap#",
    "scala/collection/mutable/LongMap#",
    "scala/collection/mutable/WeakHashMap#",
    "scala/collection/mutable/OpenHashMap#",
    "scala/collection/mutable/CollisionProofHashMap#",
    "scala/collection/mutable/PriorityQueue#",
    "scala/collection/concurrent/TrieMap#",
    "scala/collection/immutable/HashMap#",
    "scala/collection/immutable/HashSet#",
    "scala/collection/immutable/IntMap#",
    "scala/collection/immutable/LongMap#",
    "java/util/HashMap#",
    "java/util/HashSet#",
    "java/util/Hashtable#",
    "java/util/IdentityHashMap#",
    "java/util/WeakHashMap#",
    "java/util/PriorityQueue#",
    "java/util/concurrent/ConcurrentHashMap#",
    "java/util/concurrent/PriorityBlockingQueue#",
    "java/util/concurrent/DelayQueue#"
  )

  private val unorderedMatcher: SymbolMatcher = SymbolMatcher.exact(unorderedTypes*)

  // Companion object symbols (`.`-suffix) used in RHS tracking to detect e.g. mutable.HashMap.empty
  private val unorderedCompanions: Seq[String] = unorderedTypes.map(_.stripSuffix("#") + ".")
  private val companionMatcher: SymbolMatcher  = SymbolMatcher.exact(unorderedCompanions*)

  // Methods that project a collection into another iteration source with the same encounter
  // order: map views, key/value sets, iterators and Java collection converters.
  private val projectionMethods: Set[String] =
    Set("values", "keys", "keySet", "entrySet", "iterator", "keysIterator", "valuesIterator", "asScala")

  // Order-sensitive operations flagged at call sites on unordered receivers (alphabetical —
  // add new entries anywhere in the list). An operation is flagged when its result embeds the
  // receiver's encounter order or when its side effects execute in encounter order:
  //   - materialization (toList, mkString, ...): output order IS the encounter order
  //   - reductions (fold, sum, minBy, ...): folds with a non-associative combining function
  //     produce order-dependent results, and min/max-style selection keeps the first best
  //     element in encounter order — observable when the Ordering equates distinct elements
  //     (e.g. Ordering.by(_.age)); a linter cannot distinguish by the implicit Ordering in
  //     scope, so min/max are included conservatively
  //   - position-based selection (find, head, ...) and regrouping (zip, grouped, ...)
  //   - derivations whose contents depend on source order (map, filter, take, ...)
  // distinct is deliberately absent: its result contents do not depend on encounter order.
  // Suppression via `// scalafix:ok UnorderedIteration` is available for documented exceptions.
  private val flaggedOperations: Set[String] = Set(
    "collect",
    "collectFirst",
    "drop",
    "dropWhile",
    "filter",
    "filterNot",
    "find",
    "flatMap",
    "fold",
    "foldLeft",
    "foldRight",
    "foreach",
    "grouped",
    "head",
    "headOption",
    "last",
    "lastOption",
    "map",
    "max",
    "maxBy",
    "maxOption",
    "min",
    "minBy",
    "minOption",
    "mkString",
    "product",
    "reduce",
    "reduceLeft",
    "reduceOption",
    "reduceRight",
    "scanLeft",
    "scanRight",
    "slice",
    "sliding",
    "splitAt",
    "sum",
    "take",
    "takeWhile",
    "toArray",
    "toIndexedSeq",
    "toList",
    "toSeq",
    "toString",
    "toVector",
    "withFilter",
    "zip",
    "zipAll",
    "zipWithIndex"
  )

  // Operations that return a new iteration source in the receiver's encounter order, so
  // order-taint propagates into derived vals (val sub = it.take(3)); includes distinct,
  // which propagates taint although it is not flagged itself. Materializing ops (toList, ...)
  // deliberately do NOT propagate: they are flagged at the call site, and re-flagging every
  // downstream use of the result would double-report. Same-type results (hashMap.filter)
  // need no propagation either: the result type matches unorderedTypes.
  private val orderPreservingOps: Set[String] = Set(
    "collect",
    "distinct",
    "drop",
    "dropWhile",
    "filter",
    "filterNot",
    "flatMap",
    "map",
    "slice",
    "take",
    "takeWhile",
    "withFilter"
  )

  // Deliberately absent from the flagged operations: sorted/sortWith/sortBy. They are
  // SeqOps-only methods, so they can never appear on the unordered types above. Where they
  // do meet unordered data, the materializing call is already the single flagged entry
  // point (hashSet.toList.sorted flags toList). Note that sorting only sanitizes
  // non-determinism when the ordering distinguishes all elements: Scala's sort is stable,
  // so hashMap.toList.sortBy(_.value) keeps ties in their non-deterministic encounter
  // order — a remediation concern, not a detection one.

}

/** Flags iteration over unordered collections (HashMap/HashSet & friends, Java hash maps, TrieMap, ...) whose encounter
  * order is non-deterministic: downstream processing on such iteration can produce differently ordered CPG output or
  * different findings across runs.
  *
  * Flagged: order-sensitive operations (see `flaggedOperations`) on unordered receivers, one-hop projections
  * (`map.values.foreach`), infix syntax, and for-comprehension generators. Not flagged: order-independent queries
  * (`contains`, `size`, `get`, `exists`, `forall`, `count`).
  *
  * Known heuristic limits (v1):
  *   - receivers statically typed as an abstract supertype (`mutable.Map`) are only caught when initialized with a
  *     recognizable constructor/companion call in the same file
  *   - `var`s are never tracked through their initializer (reassignment can change the runtime type); a `var` declared
  *     with a concrete unordered type is still flagged via its static type
  *   - materialized derivatives are not re-flagged downstream: `val xs = set.toList` is flagged at the `toList` call,
  *     but later iterations of `xs` are not (single report at the entry point)
  *   - Java `.forEach`/`.stream()` pipelines are not covered
  *
  * Suppress a deliberate exception with `// scalafix:ok UnorderedIteration` on the same line.
  */
class UnorderedIteration extends SemanticRule("UnorderedIteration") {

  import UnorderedIteration._

  override def isLinter: Boolean = true

  // Memoized declared-type resolutions: receiver symbols repeat across call sites and files,
  // and each lookup goes through scalafix's global symbol table. Global symbols are unique
  // within a run's classpath, so they are cached run-wide; local symbols (`local0`, ...) are
  // only unique within their defining file, so they are cached per document.
  private val globalDeclaredTypeCache                                     = TrieMap.empty[Symbol, Option[Symbol]]
  private var localDeclaredTypeCache: mutable.Map[Symbol, Option[Symbol]] = mutable.Map.empty

  override def fix(implicit doc: SemanticDocument): Patch = {
    localDeclaredTypeCache = mutable.Map.empty
    val unorderedSyms: Set[Symbol] = unorderedValSymbols

    // Lints an order-sensitive operation `op` (e.g. "foreach", "<-") applied to `receiver`,
    // reporting a zero-width diagnostic at `reportAt`. Ordered and untracked receivers yield
    // Patch.empty.
    def lintReceiver(receiver: Term, reportAt: Term, op: String): Patch =
      unorderedBase(receiver, unorderedSyms) match {
        case Some((base, projection)) =>
          val path = projection.fold(op)(prefix => s"$prefix.$op")
          lintDiagnostic(reportAt, resolvedTypeName(base), path)
        case None =>
          Patch.empty
      }

    val patches = doc.tree.collect {

      // Direct call or one-hop projection: receiver.method(...) / base.projection.method(...)
      case Term.Select(receiver, method) if flaggedOperations.contains(method.value) =>
        lintReceiver(receiver, method, method.value)

      // Infix syntax: receiver method arg (e.g. hashSet foreach println)
      case Term.ApplyInfix.After_4_6_0(receiver, op, _, _) if flaggedOperations.contains(op.value) =>
        lintReceiver(receiver, op, op.value)

      // For-comprehension generator: for (x <- receiver) — including projections
      case Enumerator.Generator(_, rhs) =>
        lintReceiver(rhs, rhs, "<-")

    }

    Patch.fromIterable(patches)
  }

  // The unordered base term behind a receiver expression, plus the projection name when the
  // base is reached through a one-hop projection (hashMap.values yields (hashMap, "values")).
  // None for ordered or untracked receivers; two-hop projections (map.keys.iterator) are
  // deliberately not resolved.
  private def unorderedBase(term: Term, tracked: Set[Symbol])(implicit
    doc: SemanticDocument
  ): Option[(Term, Option[String])] =
    term match {
      case Term.Select(base, projection)
          if projectionMethods.contains(projection.value) && isUnordered(base, tracked) =>
        Some((base, Some(projection.value)))
      case other if isUnordered(other, tracked) =>
        Some((other, None))
      case _ =>
        None
    }

  // Whether a term refers to an unordered collection: either a val tainted by order-taint
  // tracking, or a term whose static type (declared val type or method return type) is unordered.
  private def isUnordered(term: Term, tracked: Set[Symbol])(implicit doc: SemanticDocument): Boolean = {
    val sym = term.symbol
    tracked.contains(sym) || unorderedTypeOf(sym).nonEmpty
  }

  // The unordered collection type behind a symbol, if any: the symbol itself (for type symbols)
  // or its static type via declaredTypeOf.
  private def unorderedTypeOf(sym: Symbol)(implicit doc: SemanticDocument): Option[Symbol] =
    if (unorderedMatcher.matches(sym)) Some(sym)
    else declaredTypeOf(sym).filter(typeSym => unorderedMatcher.matches(typeSym))

  // The static collection type behind a symbol: the declared type of a val (ValueSignature) or
  // the return type of a parameterless method (MethodSignature), so both `val m: HashMap` and
  // `def makeMap: HashMap` receivers resolve to their collection type. Memoized per symbol.
  private def declaredTypeOf(sym: Symbol)(implicit doc: SemanticDocument): Option[Symbol] =
    if (sym.isLocal) localDeclaredTypeCache.getOrElseUpdate(sym, resolveDeclaredType(sym))
    else globalDeclaredTypeCache.getOrElseUpdate(sym, resolveDeclaredType(sym))

  private def resolveDeclaredType(sym: Symbol)(implicit doc: SemanticDocument): Option[Symbol] = {
    // Scala 3 SemanticDB models the return type of a parameterless def as a by-name type
    // (def f: T is => T), so it is unwrapped here.
    def typeSymbol(tpe: SemanticType): Option[Symbol] = tpe match {
      case TypeRef(_, typeSym, _) => Some(typeSym)
      case ByNameType(inner)      => typeSymbol(inner)
      case _                      => None
    }
    sym.info.flatMap { info =>
      info.signature match {
        case ValueSignature(tpe)            => typeSymbol(tpe)
        case MethodSignature(_, _, returns) => typeSymbol(returns)
        case _                              => None
      }
    }
  }

  // Collects the symbols of all vals whose value is provably an unordered collection.
  // Symbol-keyed (not name-keyed) to prevent cross-scope false positives: two vals named
  // `coll` in different objects have distinct symbols even if they share a name.
  // Only Defn.Val is collected — `var` is excluded: reassignment can change the runtime type.
  //
  // Direct sources (RHS constructs an unordered collection or has an unordered static type)
  // seed the set; order-taint then propagates in a single reachability pass through derived
  // vals — aliases (val it2 = it), projections (val it = map.iterator) and order-preserving
  // operations (val sub = it.take(3)).
  //
  // NOTE: the testkit suite runs on the same Scala 3 SemanticDB as production, so both seed
  // paths are exercised there: construction-based RHS tracking (abstractVal, typeInferred,
  // javaHashMap fixtures) and the static-type path (the methodReceiver fixture's
  // `def makeMap: mutable.HashMap`).
  private def unorderedValSymbols(implicit doc: SemanticDocument): Set[Symbol] = {
    // Every val definition in the file as (symbol, RHS), collected once.
    val valDefs: List[(Symbol, Term)] = doc.tree.collect { case Defn.Val(_, List(Pat.Var(name)), _, rhs) =>
      (name.symbol, rhs)
    }
    if (valDefs.isEmpty) return Set.empty

    val seeds = mutable.Set.empty[Symbol]
    // Reverse dependency edges: base symbol -> vals whose RHS derives its iteration order
    // from that base (alias, projection or order-preserving operation on it).
    val dependents = mutable.Map.empty[Symbol, List[Symbol]]

    valDefs.foreach { case (sym, rhs) =>
      if (unorderedTypeOf(rhs.symbol).nonEmpty || isUnorderedConstruction(rhs)) {
        seeds += sym
      }
      derivationBase(rhs).foreach { baseSym =>
        // A base with an unordered static type taints directly; a base that is itself a
        // (possibly tainted) val taints through reachability.
        if (unorderedTypeOf(baseSym).nonEmpty) seeds += sym
        else dependents.update(baseSym, sym :: dependents.getOrElse(baseSym, Nil))
      }
    }

    // Breadth-first propagation of order-taint from the seeds along dependency edges.
    val tracked = mutable.Set.empty[Symbol] ++ seeds
    val queue   = mutable.Queue.empty[Symbol] ++ seeds
    while (queue.nonEmpty) {
      val base = queue.dequeue()
      dependents.getOrElse(base, Nil).foreach { dependent =>
        if (tracked.add(dependent)) queue.enqueue(dependent)
      }
    }
    tracked.toSet
  }

  // The symbol a val's RHS derives its iteration order from, if any: a plain alias
  // (val it2 = it), a projection (val it = map.iterator), or an order-preserving operation
  // (val sub = it.take(3), also in infix form). Parameterless order-preserving operations
  // (val sub = it.distinct) parse as a plain Select, so the Select case covers projections
  // and parameterless operations alike.
  private def derivationBase(rhs: Term)(implicit doc: SemanticDocument): Option[Symbol] =
    rhs match {
      case alias: Term.Name => Some(alias.symbol)
      case Term.Select(base, method)
          if projectionMethods.contains(method.value) || orderPreservingOps.contains(method.value) =>
        Some(base.symbol)
      case Term.Apply.After_4_6_0(Term.Select(base, op), _) if orderPreservingOps.contains(op.value) =>
        Some(base.symbol)
      case Term.ApplyInfix.After_4_6_0(base, op, _, _) if orderPreservingOps.contains(op.value) =>
        Some(base.symbol)
      case _ => None
    }

  // Matches RHS expressions that construct an unordered collection, used when the declared
  // type is abstract (e.g. val m: mutable.Map[K,V] = mutable.HashMap.empty).
  private def isUnorderedConstruction(rhs: Term)(implicit doc: SemanticDocument): Boolean =
    rhs match {
      // new java.util.HashMap() / new HashMap[K, V]() — the constructed type itself, with
      // type arguments stripped.
      case Term.New(Init.After_4_6_0(tpe, _, _)) =>
        val constructed = tpe match {
          case Type.Apply.After_4_6_0(inner, _) => inner
          case inner                            => inner
        }
        unorderedMatcher.matches(constructed.symbol)

      // Companion-object call: strip argument lists and type applications down to the callee,
      // then match the companion symbol. For a qualified callee (mutable.HashMap.empty) the
      // companion is the select's qualifier; for a plain callee (HashMap(...) via import) it is
      // the callee itself. Checking both covers qualified and imported forms uniformly.
      case other =>
        @annotation.tailrec
        def callee(term: Term): Term = term match {
          case Term.Apply.After_4_6_0(fun, _)     => callee(fun)
          case Term.ApplyType.After_4_6_0(fun, _) => callee(fun)
          case fun                                => fun
        }
        callee(other) match {
          case select @ Term.Select(qualifier, _) =>
            companionMatcher.matches(select.symbol) || companionMatcher.matches(qualifier.symbol)
          case fun =>
            companionMatcher.matches(fun.symbol)
        }
    }

  // The collection type name used in diagnostics: the receiver's declared or return type
  // (e.g. `HashMap`); falls back to the plain symbol name when no SemanticDB signature is
  // available for the receiver.
  private def resolvedTypeName(term: Term)(implicit doc: SemanticDocument): String =
    declaredTypeOf(term.symbol)
      .map(typeSym => typeSym.info.map(_.displayName).getOrElse(typeSym.displayName))
      .getOrElse(term.symbol.displayName)

  // scalafix's `scalafix:ok` escape range is end-exclusive ([owner line start, owner.end)) and
  // matches on the diagnostic's start offset, so the diagnostic is anchored at the term's start.
  private def lintDiagnostic(term: Term, typeName: String, methodName: String): Patch =
    Patch.lint(
      Diagnostic(
        id = "UnorderedIterationRule",
        message = s"Iteration over unordered collection $typeName via .$methodName — " +
          "iteration order is non-deterministic. Use a sorted or ordered collection, " +
          "or suppress with `// scalafix:ok UnorderedIteration` if order does not matter here.",
        position = term.pos
      )
    )

}
