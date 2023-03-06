package io.joern.pysrc2cpg.dataflow

import io.joern.dataflowengineoss.language.toExtendedCfgNode
import io.joern.pysrc2cpg.PySrc2CpgFixture
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.{Literal, Member}
import io.shiftleft.semanticcpg.language._

class DataFlowTests extends PySrc2CpgFixture(withOssDataflow = true) {

  "intra-procedural" in {
    val cpg: Cpg = code("""
      |a = 42
      |c = foo(a, b)
      |print(c)
      |""".stripMargin)
    val source = cpg.literal("42")
    val sink   = cpg.call.code("print.*").argument
    sink.reachableByFlows(source).size shouldBe 1
  }

  "chained call" in {
    val cpg: Cpg = code("""
      |a = 42
      |c = foo(a).bar()
      |sink(c)
      |""".stripMargin)
    def source = cpg.literal("42")
    def sink   = cpg.call("sink").argument
    sink.reachableByFlows(source).size shouldBe 1
  }

  "inter procedural call 1" in {
    val cpg: Cpg = code("""
      |def foo():
      |    return 42
      |bar = foo()
      |print(bar)
      |""".stripMargin)
    def source = cpg.literal("42")
    def sink   = cpg.call("print")
    sink.reachableByFlows(source).size shouldBe 1
  }

  "inter procedural call 2" in {
    val cpg: Cpg = code("""
      |def foo(input):
      |    sink(input)
      |def main():
      |    source = 42
      |    foo(source)
      |""".stripMargin)
    def source = cpg.literal("42")
    def sink   = cpg.call("sink")
    sink.reachableByFlows(source).size shouldBe 1
  }

  "flow from class variable to sink" in {
    val cpg: Cpg = code("""
                          |class Foo():
                          |    x = 'sensitive'
                          |    def foo(self):
                          |        sink(self.x)
                          |""".stripMargin)
    val source = cpg.member(".*x.*").l
    val sink   = cpg.call(".*sink").argument(1).l
    source.size shouldBe 1
    sink.size shouldBe 1
    sink.reachableByFlows(source).size shouldBe 1
  }

  "flow from class variable to sink in assignment" in {
    val cpg: Cpg = code("""
        |class Foo():
        |    x = 'sensitive'
        |    def foo(self):
        |        a = sink(self.x)
        |""".stripMargin)

    val source = cpg.member(".*x.*").l
    val sink   = cpg.call(".*sink").argument(1).l
    source.size shouldBe 1
    sink.size shouldBe 1
    val flows = sink.reachableByFlows(source).l
    flows.size shouldBe 1
    flows.head.elements.head match {
      case member: Member =>
        member.name shouldBe "x"
      case _ => fail()
    }
  }

  "flow from literal to class variable to sink in assignment" in {
    val cpg: Cpg = code("""
                          |class Foo():
                          |    x = 'sensitive'
                          |    def foo(self):
                          |        a = sink(self.x)
                          |""".stripMargin)

    val source = cpg.literal.code(".*sensitive.*").l
    val sink   = cpg.call(".*sink").argument(1).l
    source.size shouldBe 1
    sink.size shouldBe 1
    val flows = sink.reachableByFlows(source).l
    flows.size shouldBe 1
    flows.head.elements.head match {
      case literal: Literal =>
        literal.code shouldBe "'sensitive'"
      case _ => fail()
    }
  }

  "flow from instance variable in constructor (MEMBER) to sink" in {
    val cpg: Cpg = code("""
        |class Foo:
        |    def __init__(self):
        |        self.x = 'sensitive'
        |
        |    def foo(self):
        |        a = sink(self.x)
        |
        |""".stripMargin)

    val source = cpg.member(".*x.*").l
    val sink   = cpg.call(".*sink").argument(1).l
    source.size shouldBe 1
    sink.size shouldBe 1
    val flows = sink.reachableByFlows(source).l
    flows.size shouldBe 1
    flows.head.elements.head match {
      case member: Member =>
        member.name shouldBe "x"
      case _ => fail()
    }
  }

  "flow from literal to instance variable in constructor (MEMBER) to sink" in {
    val cpg: Cpg = code("""
                          |class Foo:
                          |    def __init__(self):
                          |        self.x = 'sensitive'
                          |
                          |    def foo(self):
                          |        a = sink(self.x)
                          |
                          |""".stripMargin)

    val source = cpg.literal.code(".*sensitive.*").l
    val sink   = cpg.call(".*sink").argument(1).l
    source.size shouldBe 1
    sink.size shouldBe 1
    val flows = sink.reachableByFlows(source).l
    flows.size shouldBe 1

    flows.head.elements.head match {
      case literal: Literal =>
        literal.code shouldBe "'sensitive'"
      case _ => fail()
    }
  }

  "flow from global variable to sink" in {
    val cpg: Cpg = code("""
        |import requests
        |url = "https://app.commissionly.io/api/public/opportunity"
        |
        |class CommissionlyClient:
        |    def post_data(self, data, accountId):
        |        r = requests.post(
        |            url,
        |            json={"isloop": True, "data": accountId},
        |            auth=data.password,
        |        )
        |
        |client = CommissionlyClient()
        |data = {"key1": "value1"}
        |accountId="sometext"
        |response = client.post_data(data, accountId)
        |""".stripMargin)
    val source = cpg.identifier(".*url.*").l
    val sink   = cpg.call("post").l
    source.size shouldBe 2
    sink.size shouldBe 1
    val flows = sink.reachableByFlows(source).l
    flows.size shouldBe 1

    val sourcel = cpg.literal(".*app.commissionly.io.*").l
    sourcel.size shouldBe 1

    val flowsl = sink.reachableByFlows(source).l
    flowsl.size shouldBe 1
  }

  "flow from function param to sink" in {
    val cpg: Cpg = code("""
      |import requests
      |
      |class TestClient:
      |    def get_event_data(self, accountId):
      |        payload = { "accountId": accountId }
      |
      |        r1 = requests.get("https://www.eventbriteapi.com/v3/users/me",
      |               params = payload
      |        )
      |
      |        r = requests.post("https://app.commissionly.io/api/public/opportunity",
      |                        json={"isloop": True, "data": chunk},
      |                        auth=(self.user, self.password)
      |        )
      |""".stripMargin)
    val sourceMember = cpg.member(".*password.*").l
    val sinkPost     = cpg.call.methodFullName(".*requests.*post.*").l
    val flowsPost    = sinkPost.reachableByFlows(sourceMember).l
    flowsPost.size shouldBe 1

    val sourceParam = cpg.identifier("accountId").l
    val sinkGet     = cpg.call.methodFullName(".*requests.*get.*").l

    val flowsGet = sinkGet.reachableByFlows(sourceParam).l
    flowsGet.size shouldBe 2
  }

}
