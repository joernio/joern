package io.shiftleft.semanticcpg.accesspath

object AccessPath {

  private val empty = new AccessPath(Elements(), List[Elements]())

  def apply(): AccessPath = empty

  def apply(elements: Elements, exclusions: Seq[Elements]): AccessPath = {
    if (elements.isEmpty && exclusions.isEmpty) {
      empty
    } else {
      new AccessPath(elements, exclusions.toList)
    }
  }

  def isExtensionExcluded(exclusions: Seq[Elements], extension: Elements): Boolean =
    exclusions.exists(e => extension.elements.startsWith(e.elements))

  /** This class contains operations on `Elements` that are only used in the `AccessPath` class and are not part of the
    * public API of `Elements`
    */
  private implicit class ElementsDecorations(el: Elements) {

    def noOvertaint(start: Int = 0, untilExclusive: Int = el.elements.length): Boolean = {
      var idx = start
      while (idx < untilExclusive) {
        el.elements(idx) match {
          case VariablePointerShift | VariableAccess => return false
          case _                                     =>
        }
        idx += 1
      }
      true
    }

    /** In all sane situations, invertibleTailLength is 0 or 1:
      *   - we don't expect <i> &, because you cannot take the address of pointer+i (can only take address of rvalue)
      *   - we don't expect & <i>: The & should have collapsed against a preceding *. An example where this occurs is
      *     (&(ptr->field))[1], which becomes ptr: * field & <2> * This reads the _next_ field: It doesn't alias with
      *     ptr->field at all, but reads the next bytes in the struct, after field.
      *
      * Such code is very un-idiomatic.
      */
    def invertibleTailLength: Int = {
      var i         = 0
      val nElements = el.elements.length - 1
      while (nElements - i > -1) {
        el.elements(nElements - i) match {
          case AddressOf | VariablePointerShift | _: PointerShift => i += 1
          case _                                                  => return i
        }
      }
      i
    }
  }

}

case class AccessPath(elements: Elements, exclusions: Seq[Elements]) {

  import AccessPath._

  def isEmpty: Boolean = this == AccessPath.empty

  private var cachedHash: Int = 0

  override def hashCode(): Int = {
    if (cachedHash == 0) {
      val computedHash = elements.hashCode() + exclusions.hashCode() ^ 0x404f92ab
      cachedHash = if (computedHash == 0) 1 else computedHash
      cachedHash
    } else cachedHash
  }

  // for handling of invertible elements, cf AccessPathAlgebra.md
  // FIXME: may need to process invertible tail of `other` better

  def ++(other: Elements): Option[AccessPath] = {
    if (isExtensionExcluded(other)) None
    else Some(AccessPath(this.elements ++ other, this.truncateExclusions(other).exclusions))
  }

  // FIXME: may need to process invertible tail of `other` better
  def ++(other: AccessPath): Option[AccessPath] = {
    (this ++ other.elements).map { appended =>
      other.exclusions.foldLeft(appended) { case (ap, ex) => ap.addExclusion(ex) }
    }
  }

  def matchFull(other: AccessPath): FullMatchResult = {
    val res = this.matchFull(other.elements)
    if (
      res.extensionDiff.isEmpty && res.stepIntoPath.isDefined && other.isExtensionExcluded(
        res.stepIntoPath.get.elements
      )
    ) {
      FullMatchResult(Some(this), None, Elements.empty)
    } else res
  }

  def matchFull(other: Elements): FullMatchResult = {
    val (matchRes, matchDiff) = this.matchAndDiff(other)
    matchRes match {
      case MatchResult.NO_MATCH =>
        FullMatchResult(Some(this), None, Elements.empty)
      case MatchResult.PREFIX_MATCH | MatchResult.EXACT_MATCH =>
        FullMatchResult(None, Some(AccessPath(matchDiff, this.exclusions)), Elements.empty)
      case MatchResult.VARIABLE_PREFIX_MATCH | MatchResult.VARIABLE_EXACT_MATCH =>
        FullMatchResult(Some(this), Some(AccessPath(matchDiff, this.exclusions)), Elements.empty)
      case MatchResult.EXTENDED_MATCH =>
        FullMatchResult(
          Some(this.addExclusion(matchDiff)),
          Some(AccessPath(Elements.empty, exclusions).truncateExclusions(matchDiff)),
          matchDiff
        )
      case MatchResult.VARIABLE_EXTENDED_MATCH =>
        FullMatchResult(
          Some(this),
          Some(AccessPath(Elements.empty, exclusions).truncateExclusions(matchDiff)),
          matchDiff
        )
    }
  }

  def matchAndDiff(other: Elements): (MatchResult.MatchResult, Elements) = {
    val thisTail  = elements.invertibleTailLength
    val otherTail = other.invertibleTailLength
    val thisHead  = elements.elements.length - thisTail
    val otherHead = other.elements.length - otherTail

    val cmpUntil    = scala.math.min(thisHead, otherHead)
    var idx         = 0
    var overTainted = false
    while (idx < cmpUntil) {
      (elements.elements(idx), other.elements(idx)) match {
        case (VariableAccess, VariableAccess) | (_: ConstantAccess, VariableAccess) |
            (VariableAccess, _: ConstantAccess) | (VariablePointerShift, VariablePointerShift) |
            (_: PointerShift, VariablePointerShift) | (VariablePointerShift, _: PointerShift) =>
          overTainted = true
        case (thisElem, otherElem) =>
          if (thisElem != otherElem)
            return (MatchResult.NO_MATCH, Elements.empty)
      }
      idx += 1
    }
    var done = false

    /** We now try to greedily match more elements. We know that one of the two paths will only contain invertible
      * elements. The issue is the following: prefix <1> & x prefix <?> & With greedy matching, we end up with a diff:
      * x. If we just did the invert-append algorithm, we would end up with a less precise diff: * <?> <1> & x == * <?>
      * & x.
      */
    val minlen = scala.math.min(elements.elements.length, other.elements.length)
    while (!done && idx < minlen) {
      (elements.elements(idx), other.elements(idx)) match {
        case (_: PointerShift, VariablePointerShift) | (VariablePointerShift, _: PointerShift) |
            (VariablePointerShift, VariablePointerShift) =>
          overTainted = true
          idx += 1
        case (thisElem, otherElem) =>
          if (thisElem == otherElem) {
            idx += 1
          } else {
            done = true
          }
      }
    }
    if (thisHead >= otherHead) {
      // prefix or exact
      val diff = Elements.inverted(other.elements.drop(idx)) ++ Elements.unnormalized(elements.elements.drop(idx))

      /** we don't need to overtaint if thisTail has variable PointerShift: They can still get excluded e.g. suppose we
        * track "a" "b" <?> and encounter "a" <4>.
        */
      overTainted |= !other.noOvertaint(otherHead)
      if (!overTainted & thisHead == otherHead) (MatchResult.EXACT_MATCH, diff)
      else if (overTainted && thisHead == otherHead) (MatchResult.VARIABLE_EXACT_MATCH, diff)
      else if (!overTainted && thisHead != otherHead) (MatchResult.PREFIX_MATCH, diff)
      else if (overTainted && thisHead != otherHead) (MatchResult.VARIABLE_PREFIX_MATCH, diff)
      else throw new RuntimeException()
    } else {
      // extended
      val diff = Elements.inverted(elements.elements.drop(idx)) ++ Elements.unnormalized(other.elements.drop(idx))

      /** we need to overtaint if any either otherTail or thisTail has variable PointerShift: e.g. suppose we track "a"
        * <4> and encounter "a" "b" <?> "c", or suppose that we track "a" <?> and encounter "a" "b"
        */
      overTainted |= !elements.noOvertaint(thisHead) | !other.noOvertaint(otherHead)

      if (overTainted) (MatchResult.VARIABLE_EXTENDED_MATCH, diff)
      else if (isExtensionExcluded(diff)) (MatchResult.NO_MATCH, Elements.empty)
      else (MatchResult.EXTENDED_MATCH, diff)
    }
  }

  private def truncateExclusions(compareExclusion: Elements): AccessPath = {
    if (exclusions.isEmpty) return this
    val size = compareExclusion.elements.length
    val newExclusions =
      exclusions
        .filter(_.elements.startsWith(compareExclusion.elements))
        .map(exclusion => Elements.normalized(exclusion.elements.drop(size)))
        .sorted
    AccessPath(elements, newExclusions)
  }

  private def addExclusion(newExclusion: Elements): AccessPath = {
    if (newExclusion.noOvertaint()) {
      val ex =
        Elements.unnormalized(newExclusion.elements.dropRight(newExclusion.invertibleTailLength))
      if (isExtensionExcluded(ex)) return this
      val unshadowed = exclusions.filter(!_.elements.startsWith(ex.elements))
      AccessPath(elements, (unshadowed :+ ex).sorted)
    } else this
  }

  def isExtensionExcluded(extension: Elements): Boolean = {
    AccessPath.isExtensionExcluded(this.exclusions, extension)
  }

}

sealed trait MatchResult
object MatchResult extends Enumeration {
  type MatchResult = Value
  val NO_MATCH, EXACT_MATCH, VARIABLE_EXACT_MATCH, PREFIX_MATCH, VARIABLE_PREFIX_MATCH, EXTENDED_MATCH,
    VARIABLE_EXTENDED_MATCH = Value
}

/** Result of `matchFull` comparison
  *
  * @param stepOverPath
  *   the unaffected part of the access path. Some(this) for no match, None for perfect match; may have additional
  *   exclusions to this.
  * @param stepIntoPath
  *   The affected part of the access path, mapped to be relative to this stepIntoPath.isDefined if and only if there is
  *   a match in paths, i.e. if the call can affect the tracked variable at all. Outside of overtainting, if
  *   stepIntoPath.isDefined && stepIntoPath.elements.nonEmpty then: path.elements == other.elements ++
  *   path.matchFull(other).stepIntoPath.get.elements extensionDiff.isEmpty
  *
  * @param extensionDiff
  *   extensionDiff is non empty if and only if a proper subset is affected. Outside of over tainting, if extensionDiff
  *   is non empty then: path.elements ++ path.matchFull(other).extensionDiff == other.elements
  *   path.matchFull(other).stepIntoPath.get.elements.isEmpty
  *
  * Invariants:
  *   - Exclusions have no invertible tail
  *   - Only paths without overTaint can have exclusions TODO: Figure out sensible assertions to defend these invariants
  */
case class FullMatchResult(
  stepOverPath: Option[AccessPath],
  stepIntoPath: Option[AccessPath],
  extensionDiff: Elements
) {
  def hasMatch: Boolean = stepIntoPath.nonEmpty
}

/** For handling of invertible elements, cf AccessPathAlgebra.md. The general rule is that elements concatenate
  * normally, except for:
  *
  * Elements(&) ++ Elements(*) == Elements() Elements(*) ++ Elements(&) == Elements() Elements(<0>) == Elements()
  * Elements(<i>) ++ Elements(<j>) == Elements(<i+j>) Elements(<?>) ++ Elements(<j>) == Elements(<?>) Elements(<i>) ++
  * Elements(<?>) == Elements(<?>) Elements(<?>) ++ Elements(<?>) == Elements(<?>)
  *
  * From this, once can see that <i>, * and & are invertible, <?> is idempotent and <0> is a convoluted way of
  * describing and empty sequence of tokens. Nevertheless, we mostly consider * as noninvertible (because it is, in real
  * computers!) and <?> as invertible (because it is in real computers, we just don't know the offset)
  *
  * Elements get a private constructor. Users should use the no-argument Elements.apply() factory method to get an empty
  * path, and the specific concat operators for building up pathes. The Elements.normalized(iter) factory method serves
  * to build this in bulk.
  *
  * The unnormalized factory method is more of an escape hatch.
  *
  * The elements field should never be mutated outside of this file: We compare and hash Elements by their contents, not
  * by identity, and this breaks in case of mutation.
  *
  * The reason for using a mutable Array instead of an immutable Vector is that this is the lightest weight
  * datastructure for the job.
  *
  * The reason for making this non-private is simply that it is truly annoying to write wrappers for all possible uses.
  */
// TODO: Figure out sensible assertions to defend invariant that the empty instance is
// the only empty elements instance, i.e., assert that elems.isEmpty implies elems eq
// Elements.empty

object Elements {
  val empty = new Elements()

  def apply(): Elements = empty

  def normalized(elems: IterableOnce[AccessElement]): Elements =
    destructiveNormalized(elems.iterator.toArray)

  def normalized(elems: AccessElement*): Elements =
    destructiveNormalized(elems.toArray)

  def unnormalized(elems: IterableOnce[AccessElement]): Elements =
    newIfNonEmpty(elems.iterator.toArray)

  def newIfNonEmpty(elems: Array[AccessElement]): Elements = {
    if (!elems.isEmpty) new Elements(elems)
    else empty
  }

  def inverted(elems: Iterable[AccessElement]): Elements = {
    val invertedElems: Array[AccessElement] = elems.toArray.reverse.map {
      case AddressOf            => IndirectionAccess
      case IndirectionAccess    => AddressOf
      case PointerShift(idx)    => PointerShift(-idx)
      case VariablePointerShift => VariablePointerShift
      case _                    => throw new RuntimeException(s"Cannot invert ${Elements.unnormalized(elems)}")
    }
    newIfNonEmpty(invertedElems)
  }

  def noOvertaint(elems: Iterable[AccessElement]): Boolean =
    elems.forall(_ != VariableAccess)

  private def destructiveNormalized(elems: Array[AccessElement]): Elements = {
    var idxRight = 0
    var idxLeft  = -1
    while (idxRight < elems.length) {
      val nextE = elems(idxRight)
      nextE match {
        case shift: PointerShift if shift.logicalOffset == 0 =>
        // nothing to do
        case _ =>
          if (idxLeft == -1) {
            idxLeft = 0
            elems(0) = nextE
          } else {
            val lastE = elems(idxLeft)
            (lastE, nextE) match {
              case (last: PointerShift, next: PointerShift) =>
                val newShift = last.logicalOffset + next.logicalOffset
                if (newShift != 0) elems(idxLeft) = PointerShift(newShift)
                else idxLeft -= 1
              case (VariablePointerShift, _: PointerShift) | (VariablePointerShift, VariablePointerShift) =>
              case (_: PointerShift, VariablePointerShift) =>
                elems(idxLeft) = VariablePointerShift
              case (AddressOf, IndirectionAccess) =>
                idxLeft -= 1
              case (IndirectionAccess, AddressOf) =>
                idxLeft -= 1 // WRONG but useful, cf comment for `Elements.:+`
              case _ =>
                idxLeft += 1
                elems(idxLeft) = nextE
            }
          }
      }
      idxRight += 1
    }
    newIfNonEmpty(elems.take(idxLeft + 1))
  }

}

final class Elements(val elements: Array[AccessElement] = Array[AccessElement]()) extends Comparable[Elements] {

  def isEmpty: Boolean = elements.isEmpty

  override def toString: String = s"Elements(${elements.mkString(",")})"

  override def equals(other: Any): Boolean = {
    other match {
      case otherElements: Elements =>
        Array.equals(elements.asInstanceOf[Array[AnyRef]], otherElements.elements.asInstanceOf[Array[AnyRef]])
      case _ => false
    }
  }

  override def hashCode(): Int = java.util.Arrays.hashCode(elements.asInstanceOf[Array[AnyRef]])

  override def compareTo(other: Elements): Int = {
    val until = scala.math.min(elements.length, other.elements.length)
    var idx   = 0
    while (idx < until) {
      elements(idx).compareTo(other.elements(idx)) match {
        case 0          =>
        case difference => return difference
      }
      idx += 1
    }
    if (idx < elements.length) +1
    else if (idx < other.elements.length) -1
    else 0
  }

  def ++(otherElements: Elements): Elements = {

    if (elements.isEmpty) return otherElements
    if (otherElements.isEmpty) return this

    var buf       = None: Option[AccessElement]
    val otherSize = otherElements.elements.length
    var idx       = 0
    val until     = scala.math.min(elements.length, otherSize)
    var done      = false

    while (idx < until & !done) {
      (elements(elements.length - idx - 1), otherElements.elements(idx)) match {
        case (AddressOf, IndirectionAccess) =>
          idx += 1
        case (IndirectionAccess, AddressOf) =>
          // WRONG but useful, cf comment for `Elements.:+`
          idx += 1
        case (VariablePointerShift, VariablePointerShift) | (_: PointerShift, VariablePointerShift) |
            (VariablePointerShift, _: PointerShift) =>
          done = true
          buf = Some(VariablePointerShift)
          idx += 1
        case (last: PointerShift, first: PointerShift) =>
          val newOffset = last.logicalOffset + first.logicalOffset
          if (newOffset != 0) {
            done = true
            buf = Some(PointerShift(newOffset))
          }
          idx += 1
        case _ =>
          done = true
      }
    }
    val sz  = elements.length + otherSize - 2 * idx + (if (buf.isDefined) 1 else 0)
    val res = Array.fill(sz) { null }: Array[AccessElement]
    elements.copyToArray(res, 0, elements.length - idx)
    if (buf.isDefined) {
      res(elements.length - idx) = buf.get
      java.lang.System.arraycopy(otherElements.elements, idx, res, elements.length - idx + 1, otherSize - idx)
    } else {
      java.lang.System.arraycopy(otherElements.elements, idx, res, elements.length - idx, otherSize - idx)
    }
    Elements.newIfNonEmpty(res)
  }

}
