package io.joern.benchmarks

import org.scalatest.Tag

import java.io.{File, PrintStream}
import scala.collection.mutable

object BenchmarkTags {

  val resultsOut: PrintStream = Option(System.getenv("JOERN_BENCHMARK_RESULT_FILE")) match {
    case Some(filePath) =>
      val f = new File(filePath)
      f.getParentFile.mkdirs()
      f.createNewFile()
      new PrintStream(f)
    case None => System.out
  }

  object Aliasing       extends Tag("Aliasing")
  object Arrays         extends Tag("Arrays")
  object Basic          extends Tag("Basic")
  object Collections    extends Tag("Collections")
  object DataStructures extends Tag("Data Structures")
  object Factories      extends Tag("Factories")
  object Inter          extends Tag("Inter-procedural")
  object Pred           extends Tag("Predicates")
  object Refl           extends Tag("Reflection")
  object Sanitizers     extends Tag("Sanitizers")
  object Session        extends Tag("Session")
  object StrongUpdates  extends Tag("Strong Updates")
  // Additional tags for IFSPEC
  object Casting          extends Tag("Casting")
  object ClassInitializer extends Tag("Class Initializer")
  object HighConditional  extends Tag("High Conditional")
  object ImplicitFlows    extends Tag("Implicit Flows")
  object Exceptions       extends Tag("Exceptions")
  object ExplicitFlows    extends Tag("Explicit Flows")
  object Library          extends Tag("Library")
  object Simple           extends Tag("Simple")
  // Additional tags for JInfoFlow
  object Context extends Tag("Context")
  object Events  extends Tag("Event")

  def TAGS: Seq[String] = Seq(
    Aliasing.name,
    Arrays.name,
    Basic.name,
    Collections.name,
    DataStructures.name,
    Factories.name,
    Inter.name,
    Pred.name,
    Refl.name,
    Sanitizers.name,
    Session.name,
    StrongUpdates.name,
    Casting.name,
    ClassInitializer.name,
    HighConditional.name,
    ImplicitFlows.name,
    Exceptions.name,
    ExplicitFlows.name,
    Library.name,
    Simple.name,
    Context.name,
    Events.name
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
    resultsOut.println(s"| Category ${catWhiteSpace.mkString} | #    | FP   | TP   | TN   | FN   |")
    resultsOut.println(
      s"| ${(for (_ <- 0 to catWhiteSpaceCount) yield '-').mkString} | ---- | ---- | ---- | ---- | ---- |"
    )
    TAGS.filter { tag => confusionMatrix(tag).sum > 0 }.foreach { tag =>
      val m = confusionMatrix(tag)
      resultsOut.println(s"| $tag ${(for (_ <- 0 until (catWhiteSpaceCount - tag.length))
          yield ' ').mkString} | ${m.sum} | ${m(FP)} | ${m(TP)} | ${m(TN)} | ${m(FN)} |")
    }
    val (totalTests, fps, tps, tns, fns) = getTotalTests
    resultsOut.println(s"| *Total* ${(for (_ <- 0 until (catWhiteSpaceCount - "*Total*".length))
        yield ' ').mkString} | *$totalTests* | *$fps* | *$tps* | *$tns* | *$fns* |\n")
    resultsOut.println(s"Total accuracy: ${String.format("%.3f", (tns + tps + 0.0) / totalTests * 100.0)}%")
  }

  private def getTotalTests: (Int, Int, Int, Int, Int) = {
    val impl = confusionMatrix.getOrElse(ImplicitFlows.name, Array.ofDim[Int](4))
    val expl = confusionMatrix.getOrElse(ExplicitFlows.name, Array.ofDim[Int](4))
    if (impl.sum > 0 || expl.sum > 0) {
      // running IFSpec so this means there is a 1-many relationship with tests-categories
      (impl.sum + expl.sum, impl(FP) + expl(FP), impl(TP) + expl(TP), impl(TN) + expl(TN), impl(FN) + expl(FN))
    } else {
      (
        confusionMatrix.map(_._2.sum).sum,
        confusionMatrix.map(_._2(FP)).sum,
        confusionMatrix.map(_._2(TP)).sum,
        confusionMatrix.map(_._2(TN)).sum,
        confusionMatrix.map(_._2(FN)).sum
      )
    }
  }

  sys.addShutdownHook(finalResults())
}
