package io.joern.x2cpg.utils

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.util.concurrent.ConcurrentLinkedQueue
import scala.jdk.CollectionConverters.*
import scala.util.Success

class ConcurrentTaskUtilTests extends AnyWordSpec with Matchers {

  "provide the means to let the caller handle unsuccessful operations without propagating an exception" in {
    val problem = Iterator(() => "Success!", () => "Success!", () => throw new RuntimeException("Failure!"))
    val result  = ConcurrentTaskUtil.runUsingThreadPool(problem)
    result.count {
      case Success(_) => true
      case _          => false
    } shouldBe 2
  }

  "parMap" should {
    "preserve the order of the input" in {
      val input = (0 until 1000).toList
      ConcurrentTaskUtil.parMap(input)(_ * 2) shouldBe input.map(_ * 2).toVector
    }

    "handle empty input" in {
      ConcurrentTaskUtil.parMap(List.empty[Int])(_ + 1) shouldBe Vector.empty
    }

    "accept any IterableOnce" in {
      ConcurrentTaskUtil.parMap(Iterator(1, 2, 3))(_ + 1) shouldBe Vector(2, 3, 4)
    }

    "run tasks on more than the calling thread" in {
      val threads = ConcurrentTaskUtil.parMap(0 until 64) { _ =>
        Thread.sleep(5)
        Thread.currentThread().getId
      }
      // Only meaningful if the machine offers parallelism at all.
      if (Runtime.getRuntime.availableProcessors() > 1) {
        threads.toSet.size should be > 1
      }
    }

    "rethrow the exception of a failing task" in {
      val ex = intercept[IllegalStateException] {
        ConcurrentTaskUtil.parMap(List(1, 2, 3)) { x =>
          if (x == 2) throw new IllegalStateException("boom") else x
        }
      }
      ex.getMessage shouldBe "boom"
    }
  }

  "parForeach" should {
    "run the function for every element" in {
      val seen = new ConcurrentLinkedQueue[Int]()
      ConcurrentTaskUtil.parForeach(0 until 100)(seen.add(_))
      seen.asScala.toSet shouldBe (0 until 100).toSet
    }

    "rethrow the exception of a failing task" in {
      intercept[IllegalStateException] {
        ConcurrentTaskUtil.parForeach(List(1, 2, 3)) { x =>
          if (x == 3) throw new IllegalStateException("boom")
        }
      }
    }
  }

}
