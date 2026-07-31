package fix

import scalafix.v1._
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

  // Materializing operations produce ordered output whose element order IS the encounter order.
  private val materializingOps: Set[String] =
    Set("toList", "toSeq", "toVector", "toArray", "toIndexedSeq", "mkString", "toString")

  // Reducing operations combine elements in encounter order: folds with a non-associative
  // combining function produce order-dependent results, and min/max-style selection keeps the
  // first best element in encounter order — observable when the Ordering equates distinct
  // elements (e.g. Ordering.by(_.age)). A linter cannot distinguish by the implicit Ordering
  // in scope, so min/max are included conservatively.
  private val reducingOps: Set[String] = Set(
    "foldLeft",
    "foldRight",
    "fold",
    "reduce",
    "reduceLeft",
    "reduceRight",
    "reduceOption",
    "scanLeft",
    "scanRight",
    "sum",
    "product",
    "min",
    "max",
    "minBy",
    "maxBy",
    "minOption",
    "maxOption"
  )

  // Selecting operations pick an element by its position in the encounter order.
  private val selectingOps: Set[String] = Set("find", "collectFirst", "head", "last", "headOption", "lastOption")

  // Regrouping operations pair or group elements by their encounter order.
  private val regroupingOps: Set[String] = Set("zip", "zipWithIndex", "zipAll", "grouped", "sliding", "splitAt")

  // Traversing operations execute a side-effecting function in encounter order.
  private val traversingOps: Set[String] = Set("foreach")

  // Operations that return a new iteration source in the receiver's encounter order (typically
  // Iterator/views), so order-taint propagates into derived vals (val sub = it.take(3)).
  // Materializing ops (toList, toSeq, ...) deliberately do NOT propagate: they are flagged at the
  // call site themselves, and re-flagging every downstream use of the result would double-report.
  // Same-type ops (e.g. hashMap.filter) need no propagation: the result type matches unorderedTypes.
  private val orderPreservingOps: Set[String] = Set(
    "take",
    "drop",
    "slice",
    "takeWhile",
    "dropWhile",
    "filter",
    "filterNot",
    "withFilter",
    "map",
    "collect",
    "flatMap",
    "distinct"
  )

  // Order-preserving operations whose result contents do not depend on encounter order:
  // they propagate order-taint into derived vals but are not flagged at the call site.
  private val propagatesOnlyOps: Set[String] = Set("distinct")

  // The call sites flagged by this rule: operations whose results embed encounter order or
  // carry order-dependent side effects. All order-preserving ops are flagged as well — the
  // derived contents or their encounter order depend on the source order — minus the
  // propagate-only exceptions above.
  // Suppression via `// scalafix:ok UnorderedIteration` is available for documented exceptions.
  private val flaggedOperations: Set[String] =
    materializingOps ++ reducingOps ++ selectingOps ++ regroupingOps ++ traversingOps ++
      (orderPreservingOps -- propagatesOnlyOps)

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

  override def fix(implicit doc: SemanticDocument): Patch = {
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

  // Whether a term refers to an unordered collection: either a val tainted by fixpoint
  // tracking, or a term whose static type (declared val type or method return type) is unordered.
  private def isUnordered(term: Term, tracked: Set[Symbol])(implicit doc: SemanticDocument): Boolean =
    tracked.contains(term.symbol) || unorderedTypeOf(term.symbol).nonEmpty

  // The unordered collection type behind a symbol, if any: the symbol itself (for type symbols)
  // or its static type via declaredTypeOf.
  private def unorderedTypeOf(sym: Symbol)(implicit doc: SemanticDocument): Option[Symbol] =
    if (unorderedMatcher.matches(sym)) Some(sym)
    else declaredTypeOf(sym).filter(typeSym => unorderedMatcher.matches(typeSym))

  // The static collection type behind a symbol: the declared type of a val (ValueSignature) or
  // the return type of a parameterless method (MethodSignature), so both `val m: HashMap` and
  // `def makeMap: HashMap` receivers resolve to their collection type. Scala 3 SemanticDB
  // models the return type of a parameterless def as a by-name type (def f: T is => T),
  // so it is unwrapped here.
  private def declaredTypeOf(sym: Symbol)(implicit doc: SemanticDocument): Option[Symbol] = {
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
  // seed the set; a fixpoint pass then propagates order-taint through derived vals — aliases
  // (val it2 = it), projections (val it = map.iterator) and order-preserving operations
  // (val sub = it.take(3)) — until no new tainted vals are found.
  //
  // NOTE: the testkit suite runs on the same Scala 3 SemanticDB as production, so both seed
  // paths are exercised there: construction-based RHS tracking (abstractVal, typeInferred,
  // javaHashMap fixtures) and the static-type path (the methodReceiver fixture's
  // `def makeMap: mutable.HashMap`).
  private def unorderedValSymbols(implicit doc: SemanticDocument): Set[Symbol] = {
    // Every val definition in the file as (symbol, RHS), collected once and reused by all
    // fixpoint rounds.
    val valDefs: List[(Symbol, Term)] = doc.tree.collect { case Defn.Val(_, List(Pat.Var(name)), _, rhs) =>
      (name.symbol, rhs)
    }

    def isDirectSource(rhs: Term): Boolean =
      unorderedTypeOf(rhs.symbol).nonEmpty || isUnorderedConstruction(rhs)

    var tracked: Set[Symbol] = valDefs.collect { case (sym, rhs) if isDirectSource(rhs) => sym }.toSet
    var changed              = true
    while (changed) {
      val newSyms = valDefs.collect {
        case (sym, rhs) if !tracked.contains(sym) && derivesFromTracked(rhs, tracked) => sym
      }
      changed = newSyms.nonEmpty
      tracked ++= newSyms
    }
    tracked
  }

  // Whether an RHS derives its iteration order from an already-tracked unordered val:
  // a plain alias (val it2 = it), a projection (val it = map.iterator), or an
  // order-preserving operation (val sub = it.take(3), also in infix form). Parameterless
  // order-preserving operations (val sub = it.distinct) parse as a plain Select, so the
  // Select case covers projections and parameterless operations alike.
  private def derivesFromTracked(rhs: Term, tracked: Set[Symbol])(implicit doc: SemanticDocument): Boolean =
    rhs match {
      case alias: Term.Name => isUnordered(alias, tracked)
      case Term.Select(base, method) =>
        (projectionMethods.contains(method.value) || orderPreservingOps.contains(method.value)) &&
        isUnordered(base, tracked)
      case Term.Apply.After_4_6_0(Term.Select(base, op), _) =>
        orderPreservingOps.contains(op.value) && isUnordered(base, tracked)
      case Term.ApplyInfix.After_4_6_0(base, op, _, _) =>
        orderPreservingOps.contains(op.value) && isUnordered(base, tracked)
      case _ => false
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

  // Note: the diagnostic is a zero-width position at the term's end, i.e. it is reported on the
  // expression's last line — for multi-line calls, suppression comments must be placed there.
  private def lintDiagnostic(term: Term, typeName: String, methodName: String): Patch =
    Patch.lint(
      Diagnostic(
        id = "UnorderedIterationRule",
        message = s"Iteration over unordered collection $typeName via .$methodName — " +
          "iteration order is non-deterministic. Use a sorted or ordered collection, " +
          "or suppress with `// scalafix:ok UnorderedIteration` if order does not matter here.",
        position = Position.Range(term.pos.input, term.pos.end, term.pos.end)
      )
    )

}
