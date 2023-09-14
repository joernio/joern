package io.joern.x2cpg.utils

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class KeyPoolTests extends AnyWordSpec with Matchers {

  "IntervalKeyPool" should {
    "return [first, ..., last] and then raise" in {
      val keyPool = new IntervalKeyPool(10, 19)
      List.range(0, 10).map(_ => keyPool.next) shouldBe List.range(10, 20)
      assertThrows[RuntimeException] { keyPool.next }
      assertThrows[RuntimeException] { keyPool.next }
    }

    "allow splitting into multiple pools" in {
      val keyPool = new IntervalKeyPool(1, 1000)
      val pools   = keyPool.split(11).toList
      assertThrows[IllegalStateException] { keyPool.next }
      pools.size shouldBe 11
      // Pools should all have the same size
      pools
        .map { x =>
          (x.last - x.first)
        }
        .distinct
        .size shouldBe 1
      // Pools should be pairwise disjoint
      val keySets = pools.map { x =>
        (x.first to x.last).toSet
      }
      keySets.combinations(2).foreach {
        case List(x: Set[Long], y: Set[Long]) =>
          x.intersect(y).isEmpty shouldBe true
        case _ =>
          fail()
      }
    }

    "return empty iterator when asked to create 0 partitions" in {
      val keyPool = new IntervalKeyPool(1, 1000)
      keyPool.split(0).hasNext shouldBe false
    }

  }

  "SequenceKeyPool" should {
    "return elements of sequence one by one and then raise" in {
      val seq     = List[Long](1, 2, 3)
      val keyPool = new SequenceKeyPool(seq)
      List.range(0, 3).map(_ => keyPool.next) shouldBe seq
      assertThrows[RuntimeException] { keyPool.next }
      assertThrows[RuntimeException] { keyPool.next }
    }
  }

  "KeyPoolCreator" should {
    "split into n pools and honor minimum value" in {
      val minValue = 10
      val pools    = KeyPoolCreator.obtain(3, minValue)
      pools.size shouldBe 3
      pools match {
        case List(pool1, pool2, pool3) =>
          pool1.first shouldBe minValue
          pool1.last should be < pool2.first
          pool2.last should be < pool3.first
          pool3.last shouldBe Long.MaxValue - 1
        case _ => fail()
      }
    }

  }

}
