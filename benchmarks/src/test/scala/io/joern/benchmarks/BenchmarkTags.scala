package io.joern.benchmarks

import org.scalatest.Tag

import scala.collection.mutable

object BenchmarkTags {

  object Aliasing         extends Tag("Aliasing")
  object Arrays           extends Tag("Arrays")
  object Basic            extends Tag("Basic")
  object Casting          extends Tag("Casting")
  object Collections      extends Tag("Collections")
  object ClassInitializer extends Tag("Class Initializer")
  object HighConditional  extends Tag("High Conditional")
  object ImplicitFlows    extends Tag("Implicit Flows")
  object Exceptions       extends Tag("Exceptions")
  object ExplicitFlows    extends Tag("Explicit Flows")
  object Library          extends Tag("Library")
  object Simple           extends Tag("Simple")

  def TAGS = Seq(
    ExplicitFlows.name,
    ImplicitFlows.name,
    Aliasing.name,
    Arrays.name,
    Basic.name,
    Collections.name,
    HighConditional.name,
    Library.name,
    Simple.name,
    Exceptions.name,
    Casting.name,
    ClassInitializer.name
  )

  val confusionMatrix = mutable.Map.empty[String, Array[Int]]
  val FP              = 0
  val TP              = 1
  val TN              = 2
  val FN              = 3

  TAGS.foreach { tag =>
    confusionMatrix.put(tag, Array.ofDim[Int](4))
  }

  private def finalResults(): Unit = {
    val catWhiteSpaceCount = TAGS.maxBy(_.length).length
    val catWhiteSpace      = for (_ <- 0 until (catWhiteSpaceCount - "Category".length)) yield ' '
    println(s"| Category ${catWhiteSpace.mkString} | #    | FP   | TP   | TN   | FN   |")
    println(s"| ${(for (_ <- 0 to catWhiteSpaceCount) yield '-').mkString} | ---- | ---- | ---- | ---- | ---- |")
    TAGS.filter { tag => confusionMatrix(tag).sum > 0 }.foreach { tag =>
      val m = confusionMatrix(tag)
      println(s"| $tag ${(for (_ <- 0 until (catWhiteSpaceCount - tag.length))
          yield ' ').mkString} | ${m.sum}   | ${countToString(m(FP))}   | ${countToString(m(TP))}   | ${countToString(
          m(TN)
        )}   | ${countToString(m(FN))}   |")
    }
    val FNs        = confusionMatrix.map(_._2(FN)).sum
    val TNs        = confusionMatrix.map(_._2(TN)).sum
    val FPs        = confusionMatrix.map(_._2(FP)).sum
    val TPs        = confusionMatrix.map(_._2(TP)).sum
    val totalTests = confusionMatrix.map(_._2.sum).sum
    println(s"| *Total* ${(for (_ <- 0 until (catWhiteSpaceCount - "*Total*".length))
        yield ' ').mkString} | *$totalTests* | *${countToString(FPs)}* | *${countToString(TPs)}* | *${countToString(TNs)}* | *${countToString(FNs)}* |")
    println(s"Total accuracy: ${String.format("%.3f", (TNs + TPs + 0.0) / totalTests * 100.0)}%")
  }

  private def countToString(count: Int): String = if (count < 10) s"$count " else count.toString

  sys.addShutdownHook(finalResults())
}
