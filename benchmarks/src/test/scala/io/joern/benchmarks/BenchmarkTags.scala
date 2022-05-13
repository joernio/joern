package io.joern.benchmarks

import org.scalatest.Tag

import java.io.{File, PrintStream, PrintWriter}
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

  def TAGS = Seq(
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
    resultsOut.println(s"| Category ${catWhiteSpace.mkString} | #    | FP   | TP   | TN   | FN   |")
    resultsOut.println(
      s"| ${(for (_ <- 0 to catWhiteSpaceCount) yield '-').mkString} | ---- | ---- | ---- | ---- | ---- |"
    )
    TAGS.filter { tag => confusionMatrix(tag).sum > 0 }.foreach { tag =>
      val m = confusionMatrix(tag)
      resultsOut.println(s"| $tag ${(for (_ <- 0 until (catWhiteSpaceCount - tag.length))
          yield ' ').mkString} | ${m.sum} | ${m(FP)} | ${m(TP)} | ${m(TN)} | ${m(FN)} |")
    }
    val FNs        = confusionMatrix.map(_._2(FN)).sum
    val TNs        = confusionMatrix.map(_._2(TN)).sum
    val FPs        = confusionMatrix.map(_._2(FP)).sum
    val TPs        = confusionMatrix.map(_._2(TP)).sum
    val totalTests = confusionMatrix.map(_._2.sum).sum
    resultsOut.println(s"| *Total* ${(for (_ <- 0 until (catWhiteSpaceCount - "*Total*".length))
        yield ' ').mkString} | *$totalTests* | *$FPs* | *$TPs* | *$TNs* | *$FNs* |\n")
    resultsOut.println(s"Total accuracy: ${String.format("%.3f", (TNs + TPs + 0.0) / totalTests * 100.0)}%")
  }

  sys.addShutdownHook(finalResults())
}
