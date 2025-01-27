package io.shiftleft.semanticcpg.language

import flatgraph.{DiffGraphApplier, DiffGraphBuilder}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.codepropertygraph.generated.nodes.{NewFinding, NewKeyValuePair, NewMethod}

class SarifTests extends AnyWordSpec with Matchers {

  "a CPG without finding nodes" should {
    val cpg = Cpg.empty

    "create a SARIF file with empty results" in {
      val sarif = cpg.finding.toSarif
      sarif.version shouldBe "2.1.0"
      sarif.schema shouldBe "https://docs.oasis-open.org/sarif/sarif/v2.1.0/errata01/os/schemas/sarif-schema-2.1.0.json"
      sarif.runs.size shouldBe 1
      val run = sarif.runs.head
      run.results shouldBe Nil
      val tool = run.tool.driver
      tool.name shouldBe "Joern"
      tool.fullName shouldBe "Joern - The Bug Hunter's Workbench"
      tool.organization shouldBe "Joern.io"
    }
  }

  "an iterable with a single finding node with all expected properties" should {

    val cpg = Cpg.empty

    val dg = Cpg.newDiffGraphBuilder
    val method = NewMethod()
      .name("Foo")
      .lineNumber(2)
      .filename("Bar.java")
      .code("public foo()")
    val finding = NewFinding()
      .evidence(Iterator.single(method))
      .keyValuePairs(
        List(
          NewKeyValuePair().key("name").value("f1"),
          NewKeyValuePair().key("title").value("Finding 1"),
          NewKeyValuePair().key("description").value("something bad happened"),
          NewKeyValuePair().key("score").value("8.0")
        )
      )
    dg.addNode(method)
      .addNode(finding)

    DiffGraphApplier.applyDiff(cpg.graph, dg)

    "create a valid SARIF result" in {
      val sarif   = cpg.finding.toSarif()
      val results = sarif.runs.head.results
      results.size shouldBe 1
      val result = results.head

      result.ruleId shouldBe "f1"
      result.message.text shouldBe "Finding 1"
      result.level shouldBe "error"

      val region = result.locations.head.physicalLocation.region

      region.startLine shouldBe Some(2)
      region.snippet.map(_.text) shouldBe Some("public foo()")

      val artifactLocation = result.locations.head.physicalLocation.artifactLocation
      artifactLocation.uri.map(_.toString) shouldBe Some("Bar.java")

      result.codeFlows.size shouldBe 1
      result.codeFlows.head.message.text shouldBe "something bad happened"
    }

  }

  "an iterable with a single finding node with missing properties" should {

    val cpg = Cpg.empty

    val dg = Cpg.newDiffGraphBuilder
    val method = NewMethod()
      .name("Foo")
      .lineNumber(2)
      .code("public foo()")
    val finding = NewFinding()
      .evidence(Iterator.single(method))
      .keyValuePairs(
        List(
          NewKeyValuePair().key("name").value("f1"),
          NewKeyValuePair().key("description").value("something bad happened")
        )
      )
    dg.addNode(method)
      .addNode(finding)

    DiffGraphApplier.applyDiff(cpg.graph, dg)

    "create a valid SARIF result" in {
      val sarif   = cpg.finding.toSarif()
      val results = sarif.runs.head.results
      results.size shouldBe 1
      val result = results.head

      result.ruleId shouldBe "f1"
      result.message.text shouldBe "<empty>"
      result.level shouldBe "warning"

      val region = result.locations.head.physicalLocation.region

      region.startLine shouldBe Some(2)
      region.snippet.map(_.text) shouldBe Some("public foo()")

      val artifactLocation = result.locations.head.physicalLocation.artifactLocation
      artifactLocation.uri.map(_.toString) shouldBe None

      result.codeFlows.size shouldBe 1
      result.codeFlows.head.message.text shouldBe "something bad happened"
    }

  }

}
