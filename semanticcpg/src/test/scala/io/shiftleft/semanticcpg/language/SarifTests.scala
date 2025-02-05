package io.shiftleft.semanticcpg.language

import flatgraph.DiffGraphApplier
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.{NewFinding, NewKeyValuePair, NewMethod}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class SarifTests extends AnyWordSpec with Matchers {

  import SarifTests.*

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
      tool.fullName shouldBe Option("Joern - The Bug Hunter's Workbench")
      tool.organization shouldBe Option("Joern.io")
    }
  }

  "an iterable with a single finding node with all expected properties" should {

    val cpg = Cpg.empty

    createValidFindingNode(cpg)

    "create a valid SARIF result" in {
      val sarif = cpg.finding.toSarif()
      val run   = sarif.runs.head
      val rules = run.tool.driver.rules

      rules.size shouldBe 1
      val rule = rules.head
      rule.id shouldBe "f1"
      rule.name shouldBe "Rule 1"
      rule.shortDescription shouldBe None
      rule.fullDescription.map(_.text) shouldBe Some("something bad happened")
      rule.helpUri shouldBe None

      val results = run.results
      results.size shouldBe 1

      val result = results.head

      result.ruleId shouldBe "f1"
      result.message.text shouldBe "Rule 1"
      result.level shouldBe "error"

      val region = result.locations.head.physicalLocation.region

      region.startLine shouldBe Some(2)
      region.snippet.map(_.text) shouldBe Some("public foo()")

      val artifactLocation = result.locations.head.physicalLocation.artifactLocation
      artifactLocation.uri.map(_.toString) shouldBe Some("Bar.java")

      result.codeFlows.size shouldBe 1
      result.codeFlows.head.message shouldBe None
    }

    "create a valid SARIF JSON" in {
      cpg.finding.toSarifJson(pretty = true) shouldBe
        """{
          |  "version":"2.1.0",
          |  "$schema":"https://docs.oasis-open.org/sarif/sarif/v2.1.0/errata01/os/schemas/sarif-schema-2.1.0.json",
          |  "runs":[
          |    {
          |      "tool":{
          |        "driver":{
          |          "organization":"Joern.io",
          |          "name":"Joern",
          |          "informationUri":"https://joern.io",
          |          "fullName":"Joern - The Bug Hunter's Workbench",
          |          "rules":[
          |            {
          |              "id":"f1",
          |              "name":"Rule 1",
          |              "fullDescription":{
          |                "text":"something bad happened"
          |              }
          |            }
          |          ]
          |        }
          |      },
          |      "results":[
          |        {
          |          "locations":[
          |            {
          |              "physicalLocation":{
          |                "artifactLocation":{
          |                  "uri":"Bar.java",
          |                  "uriBaseId":"PROJECT_ROOT"
          |                },
          |                "region":{
          |                  "startLine":2,
          |                  "snippet":{
          |                    "text":"public foo()"
          |                  }
          |                }
          |              }
          |            }
          |          ],
          |          "relatedLocations":[
          |            {
          |              "physicalLocation":{
          |                "artifactLocation":{
          |                  "uri":"Bar.java",
          |                  "uriBaseId":"PROJECT_ROOT"
          |                },
          |                "region":{
          |                  "startLine":2,
          |                  "snippet":{
          |                    "text":"public foo()"
          |                  }
          |                }
          |              }
          |            }
          |          ],
          |          "message":{
          |            "text":"Rule 1"
          |          },
          |          "codeFlows":[
          |            {
          |              "threadFlows":[
          |                {
          |                  "locations":[
          |                    {
          |                      "location":{
          |                        "physicalLocation":{
          |                          "artifactLocation":{
          |                            "uri":"Bar.java",
          |                            "uriBaseId":"PROJECT_ROOT"
          |                          },
          |                          "region":{
          |                            "startLine":2,
          |                            "snippet":{
          |                              "text":"public foo()"
          |                            }
          |                          }
          |                        }
          |                      }
          |                    }
          |                  ]
          |                }
          |              ]
          |            }
          |          ],
          |          "ruleId":"f1",
          |          "level":"error"
          |        }
          |      ],
          |      "originalUriBaseIds":{
          |        "PROJECT_ROOT":{
          |          "uriBaseId":"<empty>"
          |        }
          |      }
          |    }
          |  ]
          |}
          |
          |""".stripMargin.trim
    }

  }

  "an iterable with a single finding node with missing properties" should {

    val cpg = Cpg.empty

    createInvalidFindingNode(cpg)

    "create a valid SARIF result" in {
      val sarif = cpg.finding.toSarif()
      val run   = sarif.runs.head
      val rules = run.tool.driver.rules

      rules.size shouldBe 1
      val rule = rules.head
      rule.id shouldBe "f1"
      rule.name shouldBe "<empty>"
      rule.shortDescription shouldBe None
      rule.fullDescription.map(_.text) shouldBe Some("something bad happened")
      rule.helpUri shouldBe None

      val results = run.results
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
      result.codeFlows.head.message shouldBe None
    }

    "create a valid SARIF JSON" in {
      cpg.finding.toSarifJson(pretty = true) shouldBe
        """
          |{
          |  "version":"2.1.0",
          |  "$schema":"https://docs.oasis-open.org/sarif/sarif/v2.1.0/errata01/os/schemas/sarif-schema-2.1.0.json",
          |  "runs":[
          |    {
          |      "tool":{
          |        "driver":{
          |          "organization":"Joern.io",
          |          "name":"Joern",
          |          "informationUri":"https://joern.io",
          |          "fullName":"Joern - The Bug Hunter's Workbench",
          |          "rules":[
          |            {
          |              "id":"f1",
          |              "name":"<empty>",
          |              "fullDescription":{
          |                "text":"something bad happened"
          |              }
          |            }
          |          ]
          |        }
          |      },
          |      "results":[
          |        {
          |          "locations":[
          |            {
          |              "physicalLocation":{
          |                "artifactLocation":{
          |                  "uriBaseId":"PROJECT_ROOT"
          |                },
          |                "region":{
          |                  "startLine":2,
          |                  "snippet":{
          |                    "text":"public foo()"
          |                  }
          |                }
          |              }
          |            }
          |          ],
          |          "relatedLocations":[
          |            {
          |              "physicalLocation":{
          |                "artifactLocation":{
          |                  "uriBaseId":"PROJECT_ROOT"
          |                },
          |                "region":{
          |                  "startLine":2,
          |                  "snippet":{
          |                    "text":"public foo()"
          |                  }
          |                }
          |              }
          |            }
          |          ],
          |          "message":{
          |            "text":"<empty>"
          |          },
          |          "codeFlows":[
          |            {
          |              "threadFlows":[
          |                {
          |                  "locations":[
          |                    {
          |                      "location":{
          |                        "physicalLocation":{
          |                          "artifactLocation":{
          |                            "uriBaseId":"PROJECT_ROOT"
          |                          },
          |                          "region":{
          |                            "startLine":2,
          |                            "snippet":{
          |                              "text":"public foo()"
          |                            }
          |                          }
          |                        }
          |                      }
          |                    }
          |                  ]
          |                }
          |              ]
          |            }
          |          ],
          |          "ruleId":"f1",
          |          "level":"warning"
          |        }
          |      ],
          |      "originalUriBaseIds":{
          |        "PROJECT_ROOT":{
          |          "uriBaseId":"<empty>"
          |        }
          |      }
          |    }
          |  ]
          |}
          |""".stripMargin.trim
    }

  }

}

object SarifTests {

  def createValidFindingNode(cpg: Cpg): Unit = {
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
          NewKeyValuePair().key("title").value("Rule 1"),
          NewKeyValuePair().key("description").value("something bad happened"),
          NewKeyValuePair().key("score").value("8.0")
        )
      )
    dg.addNode(method)
      .addNode(finding)

    DiffGraphApplier.applyDiff(cpg.graph, dg)
  }

  def createInvalidFindingNode(cpg: Cpg): Unit = {
    val dg = Cpg.newDiffGraphBuilder
    val method = NewMethod()
      .name("Foo")
      .lineNumber(2)
      .code("public foo()")
      .filename("not compliant uri")
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
  }

}
