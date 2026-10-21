package io.joern.ghidra2cpg

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.util.concurrent.{Callable, Executors, TimeUnit}
import scala.jdk.CollectionConverters.*

class TypesTests extends AnyWordSpec with Matchers {

  "Types.registerType" should {
    "keep all types when called concurrently" in {
      val threads   = 8
      val perThread = 5000
      val pool      = Executors.newFixedThreadPool(threads)
      try {
        val tasks = (0 until threads).map { t =>
          new Callable[Unit] {
            override def call(): Unit = (0 until perThread).foreach(i => Types.registerType(s"concurrent-$t-$i"))
          }
        }
        val futures = pool.invokeAll(tasks.asJava, 60, TimeUnit.SECONDS).asScala.toList
        futures.foreach(_.get())
        val registered = Types.types.iterator().asScala.filter(_.startsWith("concurrent-")).toList
        registered.size shouldBe threads * perThread
        registered shouldBe registered.sorted
      } finally pool.shutdownNow()
    }
  }
}
