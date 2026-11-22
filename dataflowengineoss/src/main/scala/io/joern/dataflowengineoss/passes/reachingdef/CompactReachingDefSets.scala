package io.joern.dataflowengineoss.passes.reachingdef

import io.shiftleft.codepropertygraph.generated.nodes.CfgNode

import scala.collection.{Set, mutable}

/** How [[DataFlowSolver]] solves a problem of [[ReachingDefProblem.create]]: it finds the least fixpoint of `in(n)` = ∪
  * `out(p)` over `pred(n)` and `out(n)` = `gen(n)` ∪ (`in(n)` \ `kill(n)`) with a work list:
  *
  *   - the definitions some node generates get positions 0 to their count minus one, in the problem's order, and a set
  *     holds one bit per position, in chunks of a fixed number of words; a set is an array of references to chunks,
  *     with `null` for a chunk without definitions, and the sets live in arrays indexed by node number;
  *   - a chunk is never changed once a set holds it, so sets share chunks: a step that changes a chunk of its set puts
  *     a chunk with the new value in its place, and a chunk equal to one the step reads is shared rather than copied;
  *     `out(n)` shares every chunk of `in(n)` that `gen(n)` and `kill(n)` do not touch;
  *   - one set is shared where two sets are equal by definition: a node with a single predecessor `p` has `in(n)` =
  *     `out(p)`, and a node that neither generates nor kills has `out(n)` = `in(n)`;
  *   - a call kills through one set per variable it defines, shared by every call that defines that variable
  *     ([[ReachingDefTransferFunction.killMasks]]);
  *   - only the steps that compute something (a union at a node with several predecessors, a transfer at a node that
  *     generates or kills) are in the work list, and a step is queued again only when a set it reads grew.
  *
  * The layout of a set, with `chunkWords` = `1 << chunkShift` (32 by default):
  * {{{
  *   position p:  word w = p >>> 6, bit = p & 63
  *   word w:      chunk = w >>> chunkShift, word in the chunk = w & (chunkWords - 1)
  *
  *   ChunkedSet   [ slot 0 | slot 1 | slot 2 ]   slot c holds chunk c; null: no position in chunk c
  *                    |        null      |
  *                    v                  v
  *   Chunk        [w0 ... w63]       [w0 ... w63]
  * }}}
  * A set's array stays the same object while solving, and a step changes its slots. A chunk in a set has a bit set.
  *
  * [[DataFlowSolver]] returns the sets through [[solution]]. [[DdgGenerator]] reads the `in` sets through the views of
  * [[solveIn]], which keep the positions and translate a definition only when it is read.
  */
private[reachingdef] object CompactReachingDefSets {

  /** `1 << chunkShift` words of a set: bit `b` of word `w` stands for position `64 * w + b` of the chunk. A chunk is
    * never written once a set holds it.
    */
  type Chunk = Array[Long]

  /** A set of positions: slot `c` holds chunk `c`, or `null` when the set has no position in it. */
  type ChunkedSet = Array[Chunk]

  /** log2 of the number of words in a chunk: 32 words, 2048 definitions. */
  val defaultChunkShift: Int = 5

  /** The `in` sets of `problem`'s solution, read in place: each is a view of a solved set that translates a definition
    * when it is read, and nodes that share a set share its view. A chunk holds `1 << chunkShift` words; `chunkShift` is
    * 0 to 20.
    */
  def solveIn(
    problem: DataFlowProblem[CfgNode, mutable.BitSet],
    chunkShift: Int = defaultChunkShift
  ): Map[CfgNode, Set[Definition]] = {
    val solved = solveChunks(problem, chunkShift)
    import solved.*
    val views = new java.util.IdentityHashMap[ChunkedSet, ChunkedDefinitions]()

    def viewOf(set: ChunkedSet): Set[Definition] = {
      val existing = views.get(set)
      if (existing != null) existing
      else {
        val created = new ChunkedDefinitions(set, chunkShift, definitions, positionOf)
        views.put(set, created)
        created
      }
    }
    nodes.indices.map(number => nodes(number) -> viewOf(inSet(number))).toMap.withDefaultValue(Set.empty)
  }

  /** `problem`'s solution, read in place: `in` and `out` build a node's set when it is read. */
  def solution(problem: DataFlowProblem[CfgNode, mutable.BitSet]): Solution[CfgNode, mutable.BitSet] = {
    val in = new SolvedIn(solveIn(problem))
    Solution(in, new SolvedOut(in, problem), problem)
  }

  /** A solved problem: the reachable nodes in order, their `in` sets, and the positions (`definitions(p)` is the
    * definition at position `p`; `positionOf` the reverse).
    */
  private final class Solved(
    val nodes: Array[CfgNode],
    val inSet: Array[ChunkedSet],
    val definitions: Array[Definition],
    val positionOf: Array[Int]
  )

  /** The step that computes `in(number)` as the union of the `out` sets of its predecessors. */
  private inline def unionStepOf(number: Int): Int = 2 * number

  /** The step that computes `out(number)` with the transfer function. */
  private inline def transferStepOf(number: Int): Int = 2 * number + 1

  /** The number of the node whose set `step` computes. */
  private inline def nodeOf(step: Int): Int = step >> 1

  private inline def isUnionStep(step: Int): Boolean = (step & 1) == 0

  /** Solves `problem` in five phases: gives the generated definitions their positions and builds the chunked sets of
    * `gen` and the kill masks; per reachable node, shares `in` with a lone predecessor's `out` or makes a union step,
    * and shares `out` with `in` or makes a transfer step, recording which step writes each set; records which steps
    * read each set; queues every step; and runs steps until none is queued, queueing the readers of a set that grew.
    */
  private def solveChunks(problem: DataFlowProblem[CfgNode, mutable.BitSet], chunkShift: Int): Solved = {
    require(chunkShift >= 0 && chunkShift <= 20, s"chunkShift must be 0 to 20, not $chunkShift")
    val flowGraph    = problem.flowGraph.asInstanceOf[ReachingDefFlowGraph]
    val transfer     = problem.transferFunction.asInstanceOf[ReachingDefTransferFunction]
    val nodes        = flowGraph.allNodesReversePostOrder.toArray
    val numberOf     = flowGraph.nodeToNumber
    val nodeCount    = flowGraph.numberToNode.size
    val reachedCount = nodes.length

    // The flow graph numbers its reachable nodes 0 to `reachedCount` - 1 in this order, the others after them.
    // `gen` and the kill masks of each node, as the problem numbers definitions.
    val genSets  = Array.tabulate(nodeCount)(number => transfer.gen(flowGraph.numberToNode(number)))
    val killSets = Array.tabulate(nodeCount)(number => transfer.killMasks(flowGraph.numberToNode(number)))

    // Every `in` and `out` set holds only definitions some node generates, so a killed definition that no node
    // generates has no position and is dropped from the kill masks.
    val definitions = {
      val all = mutable.BitSet()
      genSets.foreach(all |= _)
      all.toArray
    }
    val positionOf = Array.fill(nodeCount)(-1)
    definitions.indices.foreach(position => positionOf(definitions(position)) = position)
    val chunkWords = 1 << chunkShift
    val wordMask   = chunkWords - 1
    val chunkBits  = chunkWords << 6
    val chunkCount = (definitions.length + chunkBits - 1) / chunkBits

    def chunksOf(set: IterableOnce[Definition]): ChunkedSet = {
      val result = new Array[Chunk](chunkCount)
      set.iterator.foreach { definition =>
        val position = if (definition < nodeCount) positionOf(definition) else -1
        if (position >= 0) {
          val word  = position >>> 6
          val chunk = word >>> chunkShift
          if (result(chunk) == null) result(chunk) = new Array[Long](chunkWords)
          result(chunk)(word & wordMask) |= 1L << (position & 63)
        }
      }
      result
    }
    val preds: Array[Array[Int]] =
      nodes.map(node => flowGraph.pred(node).iterator.flatMap(numberOf.get).toArray.distinct)

    val inSet  = new Array[ChunkedSet](nodeCount)
    val outSet = new Array[ChunkedSet](nodeCount)
    // For a transfer step: the chunks its `gen` or kill masks touch, in ascending order, `gen`'s chunk at each, and the
    // kill masks.
    val touched    = new Array[Array[Int]](reachedCount)
    val touchedGen = new Array[Array[Chunk]](reachedCount)
    val killMasks  = new Array[Array[ChunkedSet]](reachedCount)
    // A variable's mask is shared by the calls that define it, so it is translated once.
    val maskChunks = new java.util.IdentityHashMap[Array[Definition], ChunkedSet]()
    def chunksOfMask(mask: Array[Definition]): ChunkedSet = {
      val existing = maskChunks.get(mask)
      if (existing != null) {
        existing
      } else {
        val created = chunksOf(mask)
        maskChunks.put(mask, created)
        created
      }
    }
    // A node outside the reachable ones is never solved: its `out` is its `gen`.
    Range(reachedCount, nodeCount).foreach(number => outSet(number) = chunksOf(genSets(number)))

    val hasUnion    = new Array[Boolean](reachedCount)
    val hasTransfer = new Array[Boolean](reachedCount)
    // The step that writes each set, or -1 when no step writes it; a set is written by one step only.
    val inWriter  = Array.fill(reachedCount)(-1)
    val outWriter = Array.fill(nodeCount)(-1)
    Range(0, reachedCount).foreach { number =>
      val predecessors = preds(number)
      if (predecessors.isEmpty) {
        inSet(number) = new Array[Chunk](chunkCount)
      } else if (predecessors.length == 1 && outSet(predecessors(0)) != null) {
        inSet(number) = outSet(predecessors(0))
        inWriter(number) = outWriter(predecessors(0))
      } else {
        // Several predecessors, or a single one that is later in the order or is the node itself, which has no set to
        // share yet.
        inSet(number) = new Array[Chunk](chunkCount)
        hasUnion(number) = true
        inWriter(number) = unionStepOf(number)
      }
      if (genSets(number).isEmpty && killSets(number).isEmpty) {
        outSet(number) = inSet(number)
        outWriter(number) = inWriter(number)
      } else {
        val gen           = chunksOf(genSets(number))
        val masks         = killSets(number).map(chunksOfMask)
        val touchedChunks =
          Range(0, chunkCount).filter(chunk => gen(chunk) != null || masks.exists(_(chunk) != null)).toArray
        touched(number) = touchedChunks
        touchedGen(number) = touchedChunks.map(gen(_))
        killMasks(number) = masks
        outSet(number) = gen
        hasTransfer(number) = true
        outWriter(number) = transferStepOf(number)
      }
    }

    val readers                              = Array.fill(2 * reachedCount)(mutable.ArrayBuffer.empty[Int])
    def readBy(writer: Int, step: Int): Unit =
      if (writer >= 0 && !readers(writer).contains(step)) readers(writer) += step
    Range(0, reachedCount).foreach { number =>
      if (hasUnion(number)) preds(number).foreach(pred => readBy(outWriter(pred), unionStepOf(number)))
      if (hasTransfer(number)) readBy(inWriter(number), transferStepOf(number))
    }

    val queued                   = new Array[Boolean](2 * reachedCount)
    val work                     = new java.util.ArrayDeque[Integer]()
    def enqueue(step: Int): Unit = if (!queued(step)) { queued(step) = true; work.add(step) }
    Range(0, reachedCount).foreach { number =>
      if (hasUnion(number)) enqueue(unionStepOf(number))
      if (hasTransfer(number)) enqueue(transferStepOf(number))
    }
    val scratch = new Array[Long](chunkWords)
    while (!work.isEmpty) {
      val step = work.poll().intValue
      queued(step) = false
      val number = nodeOf(step)
      val grew   =
        if (isUnionStep(step)) applyUnion(inSet(number), preds(number), outSet, scratch)
        else
          applyTransfer(inSet(number), outSet(number), touched(number), touchedGen(number), killMasks(number), scratch)
      if (grew) readers(step).foreach(enqueue)
    }

    new Solved(nodes, inSet, definitions, positionOf)
  }

  /** Sets `in` to the union of `outSet(p)` over `predecessors`, chunk by chunk; true when `in` grew. */
  private def applyUnion(
    in: ChunkedSet,
    predecessors: Array[Int],
    outSet: Array[ChunkedSet],
    scratch: Chunk
  ): Boolean = {
    var grew  = false
    var chunk = 0
    while (chunk < in.length) {
      if (place(in, chunk, unionChunk(chunk, predecessors, outSet, in(chunk), scratch))) grew = true
      chunk += 1
    }
    grew
  }

  /** Chunk `chunk` of the union of `outSet(p)` over `predecessors`: the one chunk all predecessors that have one share;
    * else an existing chunk equal to the union, `current` first; else a copy of the union.
    */
  private def unionChunk(
    chunk: Int,
    predecessors: Array[Int],
    outSet: Array[ChunkedSet],
    current: Chunk,
    scratch: Chunk
  ): Chunk = {
    var single: Chunk = null
    var mixed         = false
    var index         = 0
    while (index < predecessors.length) {
      val from = outSet(predecessors(index))(chunk)
      if (from != null) {
        if (single == null) single = from else if (!(single eq from)) mixed = true
      }
      index += 1
    }
    if (!mixed) {
      single
    } else {
      java.util.Arrays.fill(scratch, 0L)
      index = 0
      while (index < predecessors.length) {
        orInto(scratch, outSet(predecessors(index))(chunk))
        index += 1
      }
      var found: Chunk = if (holds(current, scratch)) current else null
      index = 0
      while (found == null && index < predecessors.length) {
        val from = outSet(predecessors(index))(chunk)
        if (holds(from, scratch)) found = from
        index += 1
      }
      if (found != null) found else scratch.clone()
    }
  }

  /** Sets `out` to `gen` ∪ (`in` \ the kill masks) on the chunks in `touched`, and to `in`'s chunk on the others; true
    * when `out` grew. `touchedGen(k)` is `gen`'s chunk at chunk `touched(k)`.
    */
  private def applyTransfer(
    in: ChunkedSet,
    out: ChunkedSet,
    touched: Array[Int],
    touchedGen: Array[Chunk],
    masks: Array[ChunkedSet],
    scratch: Chunk
  ): Boolean = {
    var grew  = false
    var next  = 0
    var chunk = 0
    while (chunk < out.length) {
      val candidate =
        if (next < touched.length && touched(next) == chunk) {
          val computed = transferChunk(chunk, in(chunk), touchedGen(next), masks, out(chunk), scratch)
          next += 1
          computed
        } else {
          in(chunk)
        }
      if (place(out, chunk, candidate)) grew = true
      chunk += 1
    }
    grew
  }

  /** Chunk `chunk` of `gen` ∪ (`from` \ the kill masks): `null` when it is empty; else an existing chunk with its
    * value, `current` first, then `from`, then `gen`; else a copy.
    */
  private def transferChunk(
    chunk: Int,
    from: Chunk,
    gen: Chunk,
    masks: Array[ChunkedSet],
    current: Chunk,
    scratch: Chunk
  ): Chunk = {
    if (from == null) java.util.Arrays.fill(scratch, 0L) else System.arraycopy(from, 0, scratch, 0, scratch.length)
    var mask = 0
    while (mask < masks.length) {
      andNotInto(scratch, masks(mask)(chunk))
      mask += 1
    }
    orInto(scratch, gen)
    if (isZero(scratch)) null
    else if (holds(current, scratch)) current
    else if (holds(from, scratch)) from
    else if (holds(gen, scratch)) gen
    else scratch.clone()
  }

  /** Puts `candidate` in slot `chunk` of `set` unless the slot already holds its value; true when the set changed,
    * which, as sets only grow, is when it grew.
    */
  private def place(set: ChunkedSet, chunk: Int, candidate: Chunk): Boolean =
    if (java.util.Arrays.equals(set(chunk), candidate)) {
      false
    } else {
      set(chunk) = candidate
      true
    }

  /** True when `chunk` holds `value`'s words; false for a `null` chunk. */
  private inline def holds(chunk: Chunk, value: Chunk): Boolean = java.util.Arrays.equals(chunk, value)

  private inline def isZero(chunk: Chunk): Boolean = {
    var i = 0
    while (i < chunk.length && chunk(i) == 0) i += 1
    i == chunk.length
  }

  /** `target(i) |= chunk(i)` for every word; nothing for a `null` chunk. */
  private inline def orInto(target: Chunk, chunk: Chunk): Unit =
    if (chunk != null) {
      var i = 0
      while (i < target.length) { target(i) |= chunk(i); i += 1 }
    }

  /** `target(i) &= ~chunk(i)` for every word; nothing for a `null` chunk. */
  private inline def andNotInto(target: Chunk, chunk: Chunk): Unit =
    if (chunk != null) {
      var i = 0
      while (i < target.length) { target(i) &= ~chunk(i); i += 1 }
    }
}

/** The definitions of a [[CompactReachingDefSets.ChunkedSet]]: position `p` stands for definition `definitions(p)`, a
  * chunk holds `1 << chunkShift` words, and `positionOf` maps a definition back to its position, or to -1. Read only: a
  * set built from it, as by `filter`, `diff` or `concat`, is a new `mutable.BitSet`, so it holds its definitions in
  * ascending order, as this set does.
  */
private[reachingdef] final class ChunkedDefinitions(
  private[reachingdef] val chunks: CompactReachingDefSets.ChunkedSet,
  private[reachingdef] val chunkShift: Int,
  private[reachingdef] val definitions: Array[Definition],
  private[reachingdef] val positionOf: Array[Int]
) extends scala.collection.AbstractSet[Definition] {

  private val wordMask = (1 << chunkShift) - 1

  /** `definition`'s position, or -1 if no set of this solution can hold it. */
  private[reachingdef] def positionIn(definition: Definition): Int =
    if (definition >= 0 && definition < positionOf.length) positionOf(definition) else -1

  override def contains(definition: Definition): Boolean = {
    val position = positionIn(definition)
    position >= 0 && {
      val word  = position >>> 6
      val chunk = chunks(word >>> chunkShift)
      chunk != null && (chunk(word & wordMask) & (1L << (position & 63))) != 0
    }
  }

  /** The definitions in ascending order. */
  override def iterator: Iterator[Definition] = new Iterator[Definition] {
    private val wordCount = chunks.length << chunkShift
    // The word read last, counted across chunks: word `word & wordMask` of chunk `word >>> chunkShift`.
    private var word            = -1
    private var bits            = 0L
    private def advance(): Unit =
      while (bits == 0 && word + 1 < wordCount) {
        word += 1
        val chunk = chunks(word >>> chunkShift)
        if (chunk == null) word |= wordMask
        else bits = chunk(word & wordMask)
      }
    advance()
    override def hasNext: Boolean   = bits != 0
    override def next(): Definition = {
      if (bits == 0) throw new NoSuchElementException
      val bit = java.lang.Long.numberOfTrailingZeros(bits)
      bits &= bits - 1
      val definition = definitions((word << 6) + bit)
      advance()
      definition
    }
  }

  override lazy val size: Int =
    chunks.foldLeft(0)((sum, chunk) =>
      if (chunk == null) sum else chunk.foldLeft(sum)((total, word) => total + java.lang.Long.bitCount(word))
    )

  override def knownSize: Int = size

  override def isEmpty: Boolean = chunks.forall(chunk => chunk == null || chunk.forall(_ == 0))

  override def diff(that: Set[Definition]): Set[Definition] = fromSpecific(iterator.filterNot(that.contains))

  override def empty: Set[Definition] = mutable.BitSet()

  override protected def fromSpecific(elements: IterableOnce[Definition]): Set[Definition] =
    mutable.BitSet.fromSpecific(elements)

  override protected def newSpecificBuilder: mutable.Builder[Definition, Set[Definition]] = mutable.BitSet.newBuilder
}

/** The `in` or `out` sets of a solution of [[CompactReachingDefSets]], each built when it is read. A node without a set
  * has the empty set, here and in every map built from this one, as by `updated`, `removed`, `concat` or `filter`.
  */
private[reachingdef] abstract class SolvedSets extends scala.collection.immutable.AbstractMap[CfgNode, mutable.BitSet] {
  override def default(node: CfgNode): mutable.BitSet = mutable.BitSet()

  override def empty: Map[CfgNode, mutable.BitSet] =
    Map.empty[CfgNode, mutable.BitSet].withDefault(_ => mutable.BitSet())

  override def removed(node: CfgNode): Map[CfgNode, mutable.BitSet] =
    Map.from(iterator).removed(node).withDefault(_ => mutable.BitSet())

  override def updated[V1 >: mutable.BitSet](node: CfgNode, set: V1): Map[CfgNode, V1] =
    Map.from(iterator).updated(node, set).withDefault(_ => mutable.BitSet())

  override def concat[V2 >: mutable.BitSet](suffix: IterableOnce[(CfgNode, V2)]): Map[CfgNode, V2] =
    Map.from(iterator).concat(suffix).withDefault(_ => mutable.BitSet())

  override protected def fromSpecific(pairs: IterableOnce[(CfgNode, mutable.BitSet)]): Map[CfgNode, mutable.BitSet] =
    Map.from(pairs).withDefault(_ => mutable.BitSet())

  override protected def newSpecificBuilder: mutable.Builder[(CfgNode, mutable.BitSet), Map[CfgNode, mutable.BitSet]] =
    Map.newBuilder[CfgNode, mutable.BitSet].mapResult(_.withDefault(_ => mutable.BitSet()))
}

/** The `in` sets of a solution of [[CompactReachingDefSets]]: a node's set is built from its view when it is read. */
private[reachingdef] final class SolvedIn(val views: Map[CfgNode, Set[Definition]]) extends SolvedSets {
  override def get(node: CfgNode): Option[mutable.BitSet]    = views.get(node).map(mutable.BitSet.fromSpecific)
  override def iterator: Iterator[(CfgNode, mutable.BitSet)] =
    views.iterator.map((node, view) => node -> mutable.BitSet.fromSpecific(view))
  override def keysIterator: Iterator[CfgNode]                 = views.keysIterator
  override def keySet: scala.collection.immutable.Set[CfgNode] = views.keySet
  override def contains(node: CfgNode): Boolean                = views.contains(node)
  override def size: Int                                       = views.size
  override def knownSize: Int                                  = views.size
}

/** The `out` sets of a solution of [[CompactReachingDefSets]]: a node's set is built when it is read, by the transfer
  * function from its `in` set where it has one, else its `gen`.
  */
private[reachingdef] final class SolvedOut(in: SolvedIn, problem: DataFlowProblem[CfgNode, mutable.BitSet])
    extends SolvedSets {
  private val gen = problem.transferFunction.asInstanceOf[ReachingDefTransferFunction].gen
  override val keySet: scala.collection.immutable.Set[CfgNode] = gen.keySet ++ in.keySet
  override def get(node: CfgNode): Option[mutable.BitSet]      =
    if (in.contains(node)) Some(problem.transferFunction(node, in(node)))
    else gen.get(node).map(_.clone())
  override def iterator: Iterator[(CfgNode, mutable.BitSet)] = keySet.iterator.map(node => node -> get(node).get)
  override def keysIterator: Iterator[CfgNode]               = keySet.iterator
  override def contains(node: CfgNode): Boolean              = keySet.contains(node)
  override def size: Int                                     = keySet.size
  override def knownSize: Int                                = keySet.size
}

/** A fixed list of definitions, laid out as the words of one solution's sets: for each word that holds one of them, the
  * list's bits in it. A view is read with one AND per such word of a chunk the view has; a chunk the view does not have
  * is skipped. Built once per list and solution.
  */
private[reachingdef] final class ChunkedList(list: Iterable[Definition], view: ChunkedDefinitions) {
  private val positionOf = view.positionOf
  private val chunkShift = view.chunkShift
  private val wordMask   = (1 << chunkShift) - 1

  /** The words that hold a position of the list, in ascending order (`words`, counted across chunks), and the list's
    * bits in each (`bits`). Definitions that no set of the solution can hold are left out.
    */
  private val (words, bits) = {
    val positions = list.iterator.map(view.positionIn).filter(_ >= 0).toArray.distinct.sorted
    val wordsOut  = mutable.ArrayBuilder.make[Int]
    val bitsOut   = mutable.ArrayBuilder.make[Long]
    var i         = 0
    while (i < positions.length) {
      val word = positions(i) >>> 6
      var mask = 0L
      while (i < positions.length && (positions(i) >>> 6) == word) { mask |= 1L << (positions(i) & 63); i += 1 }
      wordsOut += word
      bitsOut += mask
    }
    (wordsOut.result(), bitsOut.result())
  }

  /** The number of words that hold a definition of the list: the most words a read looks at. */
  def wordCount: Int = words.length

  /** Whether `set` is a view of the solution this was laid out for: the same positions and chunk size. */
  def sameSolution(set: ChunkedDefinitions): Boolean = (set.positionOf eq positionOf) && set.chunkShift == chunkShift

  /** Calls `visit` on each definition of the list that `set` holds, by ascending position. */
  def foreachHeld(set: ChunkedDefinitions)(visit: Definition => Unit): Unit = {
    var i = 0
    while (i < words.length) {
      val slot  = words(i) >>> chunkShift
      val chunk = set.chunks(slot)
      while (i < words.length && (words(i) >>> chunkShift) == slot) {
        if (chunk != null) {
          var held = chunk(words(i) & wordMask) & bits(i)
          while (held != 0) {
            visit(set.definitions((words(i) << 6) + java.lang.Long.numberOfTrailingZeros(held)))
            held &= held - 1
          }
        }
        i += 1
      }
    }
  }
}
