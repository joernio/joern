package io.joern.dataflowengineoss.passes.reachingdef

import io.shiftleft.semanticcpg.accesspath.*
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class AliasKeysTests extends AnyWordSpec with Matchers {

  private val elements: Seq[AccessElement] =
    Seq(
      ConstantAccess("a"),
      ConstantAccess("b"),
      VariableAccess,
      IndirectionAccess,
      AddressOf,
      PointerShift(1),
      VariablePointerShift
    )

  private val paths: Seq[Elements] =
    (0 to 3)
      .flatMap(length =>
        Seq.fill(length)(elements).foldLeft(Seq(Seq.empty[AccessElement])) { (prefixes, choices) =>
          for (prefix <- prefixes; choice <- choices) yield prefix :+ choice
        }
      )
      .map(path => Elements.normalized(path))

  "AliasKeys.head" should {

    "be equal for every two access paths that match exactly" in {
      val exact = for {
        left  <- paths
        right <- paths
        if AccessPath(left, Nil).matchAndDiff(right)._1 == MatchResult.EXACT_MATCH
      } yield (left, right)
      exact should not be empty
      exact.foreach { case (left, right) => AliasKeys.head(left) shouldBe AliasKeys.head(right) }
    }
  }
}
