package io.shiftleft.fuzzyc2cpg

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.jdk.CollectionConverters._

class StableOutputTests extends AnyWordSpec with Matchers {

  def createNodeStrings(): String = {
    val projectName = "stableid"
    val dirName = String.format("src/test/resources/testcode/%s", projectName)
    val fuzzyc2Cpg = new FuzzyC2Cpg()
    val cpg = fuzzyc2Cpg.runAndOutput(Set(dirName), Set(".c", ".cc", ".cpp", ".h", ".hpp"))
    val nodes = cpg.graph.V().asScala.toList
    nodes.sortBy(_.id()).map(x => x.label + ": " + x.propertiesMap().asScala.toString).mkString("\n")
  }

  "Nodes in test graph" should {
    "should be exactly the same on ten consecutive runs" in {
      List
        .range(0, 10)
        .map { _ =>
          createNodeStrings()
        }
        .distinct
        .size shouldBe 1
    }
  }

}
