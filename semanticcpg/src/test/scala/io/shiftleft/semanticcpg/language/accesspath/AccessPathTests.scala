package io.shiftleft.semanticcpg.accesspath

import io.shiftleft.semanticcpg.accesspath.MatchResult.*
import org.scalatest.matchers.should.Matchers.*
import org.scalatest.wordspec.AnyWordSpec

class AccessPathTests extends AnyWordSpec {

  implicit class ElementsWithOperators(el: Elements) {

    def :+(element: AccessElement): Elements = {

      val elements = el.elements

      element match {
        case _ if !elements.isEmpty =>
          (elements.last, element) match {
            case (last: PointerShift, elem: PointerShift) =>
              val newOffset = last.logicalOffset + elem.logicalOffset

              if (newOffset == 0) Elements.newIfNonEmpty(elements.dropRight(1))
              else
                Elements.newIfNonEmpty(elements.updated(elements.length - 1, PointerShift(newOffset)))
            case (_: PointerShift, VariablePointerShift) =>
              new Elements(elements.updated(elements.length - 1, VariablePointerShift))
            case (VariablePointerShift, _: PointerShift) | (VariablePointerShift, VariablePointerShift) =>
              this.el
            case (AddressOf, IndirectionAccess) =>
              Elements.newIfNonEmpty(elements.dropRight(1))
            case (IndirectionAccess, AddressOf) =>
              /** We also collapse *&. This is WRONG (you cannot deref a pointer and then un-deref the result). However,
                * it is sometimes valid as a syntactic construct, and the language should make sure that this only
                * occurs in such settings. I.e. valid code should only produce such paths when their contraction is
                * valid. We still treat * as un-invertible, though!
                */
              Elements.newIfNonEmpty(elements.dropRight(1))
            case _ => Elements.newIfNonEmpty(elements :+ element)
          }
        case PointerShift(0) =>
          Elements.empty
        case _ =>
          Elements.newIfNonEmpty(Array(element))
      }
    }

  }

  implicit def convert(constant: String): ConstantAccess = {
    ConstantAccess(constant)
  }
  implicit def convert(constant: Int): PointerShift = PointerShift(constant)

  def E(elements: AccessElement*): Elements = {
    Elements.normalized(elements)
  }

  val V  = VariableAccess
  val I  = IndirectionAccess
  val A  = AddressOf
  val VP = VariablePointerShift

  "Test matchAndDiff" in {
    AccessPath(E("a"), Seq()).matchAndDiff(E("b")) shouldBe (NO_MATCH, E())
    AccessPath(E("a", "b"), Seq()).matchAndDiff(E("b", "a")) shouldBe (NO_MATCH, E())
    AccessPath(E("a", "b"), Seq()).matchAndDiff(E("a", "c")) shouldBe (NO_MATCH, E())
    AccessPath(E("a", V), Seq()).matchAndDiff(E("b", V)) shouldBe (NO_MATCH, E())
    AccessPath(E("a", V), Seq()).matchAndDiff(E("b")) shouldBe (NO_MATCH, E())
    AccessPath(E("a"), Seq()).matchAndDiff(E("b", V)) shouldBe (NO_MATCH, E())
    AccessPath(E("a", V, "b"), Seq()).matchAndDiff(E("b", V, "a")) shouldBe (NO_MATCH, E())
    AccessPath(E("a", I), Seq()).matchAndDiff(E(I)) shouldBe (NO_MATCH, E())
    AccessPath(E("a", I), Seq()).matchAndDiff(E("a", V)) shouldBe (NO_MATCH, E())

    AccessPath(E("a", "b"), Seq()).matchAndDiff(E("a")) shouldBe (PREFIX_MATCH, E("b"))
    AccessPath(E("a", V), Seq()).matchAndDiff(E("a")) shouldBe (PREFIX_MATCH, E(V))

    AccessPath(E(V, "a"), Seq()).matchAndDiff(E(V)) shouldBe (VARIABLE_PREFIX_MATCH, E("a"))

    AccessPath(E("a"), Seq()).matchAndDiff(E("a")) shouldBe (EXACT_MATCH, E())
    AccessPath(E("a", "b"), Seq()).matchAndDiff(E("a", "b")) shouldBe (EXACT_MATCH, E())

    AccessPath(E("a"), Seq()).matchAndDiff(E(V)) shouldBe (VARIABLE_EXACT_MATCH, E())
    AccessPath(E(V), Seq()).matchAndDiff(E("a")) shouldBe (VARIABLE_EXACT_MATCH, E())
    AccessPath(E("a", "b"), Seq()).matchAndDiff(E("a", V)) shouldBe (VARIABLE_EXACT_MATCH, E())
    AccessPath(E(V, "b"), Seq()).matchAndDiff(E(V, "b")) shouldBe (VARIABLE_EXACT_MATCH, E())
    AccessPath(E("a", V), Seq()).matchAndDiff(E(V, V)) shouldBe (VARIABLE_EXACT_MATCH, E())
    AccessPath(E(V, "a"), Seq()).matchAndDiff(E(V, V)) shouldBe (VARIABLE_EXACT_MATCH, E())
    AccessPath(E(V, V), Seq()).matchAndDiff(E("a", V)) shouldBe (VARIABLE_EXACT_MATCH, E())
    AccessPath(E(V, V), Seq()).matchAndDiff(E(V, "a")) shouldBe (VARIABLE_EXACT_MATCH, E())

    AccessPath(E(), Seq()).matchAndDiff(E("a")) shouldBe (EXTENDED_MATCH, E("a"))
    AccessPath(E("a"), Seq()).matchAndDiff(E("a", "b")) shouldBe (EXTENDED_MATCH, E("b"))
    AccessPath(E("a"), Seq()).matchAndDiff(E("a", V)) shouldBe (EXTENDED_MATCH, E(V))

    AccessPath(E("a"), Seq()).matchAndDiff(E(V, "b")) shouldBe (VARIABLE_EXTENDED_MATCH, E("b"))

    AccessPath(E("a"), Seq(E("b"))).matchAndDiff(E("a", "b", "c")) shouldBe (NO_MATCH, E())
    AccessPath(E("a"), Seq(E("b"))).matchAndDiff(E("a", "b", V)) shouldBe (NO_MATCH, E())
    AccessPath(E("a"), Seq(E("b", "c"))).matchAndDiff(E("a", "b")) shouldBe (EXTENDED_MATCH, E("b"))

    AccessPath(E("a"), Seq(E("b"))).matchAndDiff(E("a", "b")) shouldBe (NO_MATCH, E())
    AccessPath(E("a"), Seq(E("c"))).matchAndDiff(E("a", "b")) shouldBe (EXTENDED_MATCH, E("b"))
    AccessPath(E(V), Seq(E("b"))).matchAndDiff(E("a", "b")) shouldBe (VARIABLE_EXTENDED_MATCH, E("b"))
  }
  "Test normalization and concatenation" in {
    E(A, 0, I) shouldBe E()
    E(2, -1, "a", I, 3, -5, 2, A) shouldBe E(1, "a")
    E(2) ++ E(-1) ++ E("a") ++ E(I) ++ E(3) ++ E(-5) ++ E(2) ++ E(A) shouldBe E(1, "a")
    E(2) :+ PointerShift(-1) :+ ConstantAccess("a") :+ I :+ PointerShift(3) :+ PointerShift(-5) :+ PointerShift(
      2
    ) :+ A shouldBe E(1, "a")

    E("a", 3, A, 4, I, 4, I) ++ E(A, -4, A, -4, I, -3) shouldBe E("a")
    Elements.inverted(E("a", 3, A, 4, I, 4, I).elements.drop(1)) shouldBe E(A, -4, A, -4, I, -3)
    E(A, 1, VP, 2, I) shouldBe E(A, VP, I)
    E(I, "a", A) ++ E(I) shouldBe E(I, "a") // GEP
  }
  "Test matchAndDiff with inverses" in {
    // exact:
    AccessPath(E("a", 1, A, 2), Seq(E("c")))
      .matchAndDiff(E("a", 8, A, 16)) shouldBe (EXACT_MATCH, E(-16, I, -7, A, 2))
    AccessPath(E("a", 1, A, 2), Seq(E("c")))
      .matchAndDiff(E("a", 8, A, 16, I)) shouldBe (EXTENDED_MATCH, E(-2, I, 7, A, 16, I))
    AccessPath(E("a", 1, A, 2, I), Seq(E("c")))
      .matchAndDiff(E("a", 8, A, 16)) shouldBe (PREFIX_MATCH, E(-16, I, -7, A, 2, I))

    AccessPath(E("a", 1, A, 2), Seq(E("c")))
      .matchAndDiff(E("a", 8, A, 16)) shouldBe (EXACT_MATCH, E(-16, I, -7, A, 2))

    AccessPath(E("a", 1, A, 2), Seq(E("c")))
      .matchAndDiff(E("a", 8, A, 16, I)) shouldBe (EXTENDED_MATCH, E(-2, I, 7, A, 16, I))

    AccessPath(E("a", VP, A, 2), Seq(E("c")))
      .matchAndDiff(E("a", 8, A, 16, I)) shouldBe (VARIABLE_EXTENDED_MATCH, E(14, I))
    AccessPath(E("a", 1, A, 2), Seq(E("c")))
      .matchAndDiff(E("a", VP, A, 16, I)) shouldBe (VARIABLE_EXTENDED_MATCH, E(14, I))
    AccessPath(E("a", 1, A, 2), Seq(E("c")))
      .matchAndDiff(E("a", "b", 8, A, 16)) shouldBe (EXTENDED_MATCH, E(-2, I, -1, "b", 8, A, 16))

    AccessPath(E("a", 1, "b", A, 2), Seq(E("c")))
      .matchAndDiff(E("a", 8, A, 16)) shouldBe (PREFIX_MATCH, E(-16, I, -7, "b", A, 2))
    // revisit: could be exact
    AccessPath(E("a", VP, "b", A, 2), Seq(E("c")))
      .matchAndDiff(E("a", 8, A, 16)) shouldBe (VARIABLE_PREFIX_MATCH, E(-16, I, "b", A, 2))
    AccessPath(E("a", 1, "b", A, 2), Seq(E("c")))
      .matchAndDiff(E("a", VP, A, 16)) shouldBe (VARIABLE_PREFIX_MATCH, E(-16, I, "b", A, 2))

    // example where we track effectively nothing
    AccessPath(E("a", 1, A, 2), Seq(E(-2, I)))
      .matchAndDiff(E("a", "b", 8, A, 16)) shouldBe (NO_MATCH, E())
    // suboptimal:
    AccessPath(E("a", 1, A, 2), Seq(E(-2, I)))
      .matchAndDiff(E("a", VP, A, 16, I)) shouldBe (VARIABLE_EXTENDED_MATCH, E(14, I))
  }

  "Test matchFull" in {
    // no match
    AccessPath(E("a", "b"), Seq(E("c")))
      .matchFull(AccessPath(E("C"), Seq())) shouldBe FullMatchResult(
      stepOverPath = Some(AccessPath(E("a", "b"), Seq(E("c")))),
      stepIntoPath = None,
      extensionDiff = E()
    )
    // prefix
    AccessPath(E("a", "b"), E("c") :: Nil)
      .matchFull(E("a")) shouldBe FullMatchResult(
      stepOverPath = None,
      stepIntoPath = Some(AccessPath(E("b"), E("c") :: Nil)),
      extensionDiff = E()
    )
    // extension
    AccessPath(E("a", "b"), E("c", "d") :: Nil)
      .matchFull(AccessPath(E("a", "b", "c"), Nil)) shouldBe FullMatchResult(
      stepOverPath = Some(AccessPath(E("a", "b"), E("c") :: Nil)),
      stepIntoPath = Some(AccessPath(E(), E("d") :: Nil)),
      extensionDiff = E("c")
    )
    // rhs has exclusions
    AccessPath(E("a", "b"), E("c") :: Nil)
      .matchFull(AccessPath(E("a"), E("b") :: Nil)) shouldBe FullMatchResult(
      stepOverPath = Some(AccessPath(E("a", "b"), E("c") :: Nil)),
      stepIntoPath = None,
      extensionDiff = E()
    )
  }

}
