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
  object DataStructures   extends Tag("Data Structures")
  object Exceptions       extends Tag("Exceptions")
  object ExplicitFlows    extends Tag("Explicit Flows")
  object Factories        extends Tag("Factories")
  object HighConditional  extends Tag("High Conditional")
  object ImplicitFlows    extends Tag("Implicit Flows")
  object Inter            extends Tag("Inter-procedural")
  object Pred             extends Tag("Predicates")
  object Refl             extends Tag("Reflection")
  object Library          extends Tag("Library")
  object Sanitizers       extends Tag("Sanitizers")
  object Session          extends Tag("Session")
  object Simple           extends Tag("Simple")
  object StrongUpdates    extends Tag("Strong Updates")

  def TAGS = Seq(
    ExplicitFlows.name,
    ImplicitFlows.name,
    Aliasing.name,
    Arrays.name,
    Basic.name,
    Casting.name,
    ClassInitializer.name,
    Collections.name,
    DataStructures.name,
    Exceptions.name,
    Factories.name,
    HighConditional.name,
    Inter.name,
    Library.name,
    Pred.name,
    Refl.name,
    Sanitizers.name,
    Session.name,
    Simple.name,
    StrongUpdates.name
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
          yield ' ').mkString} | ${m.sum} | ${m(FP)} | ${m(TP)} | ${m(TN)} | ${m(FN)} |")
    }
    val FNs        = confusionMatrix.map(_._2(FN)).sum
    val TNs        = confusionMatrix.map(_._2(TN)).sum
    val FPs        = confusionMatrix.map(_._2(FP)).sum
    val TPs        = confusionMatrix.map(_._2(TP)).sum
    val totalTests = confusionMatrix.map(_._2.sum).sum
    println(s"| *Total* ${(for (_ <- 0 until (catWhiteSpaceCount - "*Total*".length))
        yield ' ').mkString} | *$totalTests* | *$FPs* | *$TPs* | *$TNs* | *$FNs* |")
    println(s"Total accuracy: ${String.format("%.3f", (TNs + TPs + 0.0) / totalTests * 100.0)}%")
  }

  sys.addShutdownHook(finalResults())
}
