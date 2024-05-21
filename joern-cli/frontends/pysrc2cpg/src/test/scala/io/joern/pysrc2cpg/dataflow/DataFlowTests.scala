package io.joern.pysrc2cpg.dataflow

import io.joern.dataflowengineoss.language.toExtendedCfgNode
import io.joern.dataflowengineoss.semanticsloader.FlowSemantic
import io.joern.pysrc2cpg.PySrc2CpgFixture
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.{Literal, Member, Method}
import io.shiftleft.semanticcpg.language.*

import java.io.File
import scala.collection.immutable.List

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

  "intra-procedural 2" in {
    val cpg = code("""
        |x = foo(20)
        |print(x)
        |""".stripMargin)
    val source     = cpg.literal("20")
    val sink       = cpg.call("print").argument
    val List(flow) = sink.reachableByFlows(source).map(flowToResultPairs).distinct.sortBy(_.length).l
    flow shouldBe List(("foo(20)", 2), ("x = foo(20)", 2), ("print(x)", 3))
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
    val sourceUrlIdentifier = cpg.identifier(".*url.*").l
    val sink                = cpg.call("post").l
    sourceUrlIdentifier.size shouldBe 2
    sink.size shouldBe 1
    sink.reachableByFlows(sourceUrlIdentifier).size shouldBe 2

    val sourceUrlLiteral = cpg.literal(".*app.commissionly.io.*").l
    sourceUrlLiteral.size shouldBe 1
    sink.reachableByFlows(sourceUrlLiteral).size shouldBe 1
  }

  "Flow correctly from parent scope to child function scope" in {
    val cpg: Cpg = code("""
        |def foo(u):
        |
        |  x = 1
        |
        |  def bar():
        |     y = x
        |     print(y)
        |     v = u
        |     debug(v)
        |
        |""".stripMargin)

    val sink1 = cpg.call("print").l
    val sink2 = cpg.call("debug").l
    sink1.size shouldBe 1
    sink2.size shouldBe 1

    val iSrc = cpg.method("foo").ast.isIdentifier.name("x").lineNumber(4).l
    iSrc.size shouldBe 1
    sink1.reachableBy(iSrc).dedup.size shouldBe 1

    val lSrc = cpg.method("foo").ast.isLiteral.code("1").lineNumber(4).l
    lSrc.size shouldBe 1
    sink1.reachableBy(lSrc).size shouldBe 1

    val pSrc = cpg.method("foo").parameter.nameExact("u").l
    pSrc.size shouldBe 1
    sink2.reachableBy(pSrc).size shouldBe 1
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

  "flow from index access to index access" in {
    val cpg: Cpg = code("""
        |
        |def foo():
        |    y = dict()
        |    y['B'] = x['A']
        |    sink(y)
        |""".stripMargin)

    val sources = cpg.identifier("x").l
    val sinks   = cpg.call("sink").argument.l
    val flows   = sinks.reachableByFlows(sources)
    flows.size shouldBe 1
  }

  "flow from expression that taints global variable to sink" in {
    val cpg: Cpg = code("""
        |d = {
        |   'x': F.sum('x'),
        |   'y': F.sum('y'),
        |   'z': F.sum('z'),
        |}
        |
        |class Foo():
        |   def foo(self):
        |       return sink(d)
        |""".stripMargin)

    val sources = cpg.call("<operator>.indexAccess").argument.isIdentifier.l
    val sinks   = cpg.call("sink").l
    sinks.reachableByFlows(sources).size should not be 0
  }

  "lookup of __init__ call" in {
    val cpg = code("""
        |from models import Foo
        |foo = Foo(x,y,z)
        |""".stripMargin)
      .moreCode(
        """
          |class Foo:
          |   def __init__(self, a, b, c):
          |      println("foo")
          |      pass
          |""".stripMargin,
        "models.py"
      )

    val List(method: Method) = cpg.identifier.name("foo").inAssignment.source.isCall.callee.l
    method.fullName shouldBe "models.py:<module>.Foo.__init__"
    val List(typeDeclFullName) = method.typeDecl.fullName.l
    typeDeclFullName shouldBe "models.py:<module>.Foo"
  }

  "lookup of __init__ call even when hidden in base class" in {
    val cpg = code("""
        |from models import Foo
        |foo = Foo(x,y,z)
        |""".stripMargin)
      .moreCode(
        """
          |class Foo(SomeType):
          |   pass
          |""".stripMargin,
        "models.py"
      )

    val List(method: Method) = cpg.identifier.name("foo").inAssignment.source.isCall.callee.l
    method.fullName shouldBe "models.py:<module>.Foo.__init__"
    val List(typeDeclFullName) = method.typeDecl.fullName.l
    typeDeclFullName shouldBe "models.py:<module>.Foo"
  }

  "flow from global variable defined in imported file and used as argument to `print`" in {
    val cpg = code("""
        |from models import FOOBAR
        |print(FOOBAR)
        |""".stripMargin)
      .moreCode(
        """
          |FOOBAR = "XYZ"
          |""".stripMargin,
        fileName = "models.py"
      )
    def sink       = cpg.call("print").argument.argumentIndex(1)
    def source     = cpg.literal("\"XYZ\"")
    val List(flow) = sink.reachableByFlows(source).map(flowToResultPairs).l
    flow shouldBe List(("FOOBAR = \"XYZ\"", 2), ("FOOBAR = import(models, FOOBAR)", 2), ("print(FOOBAR)", 3))
  }

  "flow from global variable defined in imported file and used inside a method as argument to `print`" in {
    val cpg = code("""
        |from models import FOOBAR
        |def run():
        |   print(FOOBAR)
        |""".stripMargin)
      .moreCode(
        """
          |FOOBAR = "XYZ"
          |""".stripMargin,
        fileName = "models.py"
      )

    def sink       = cpg.call("print").argument.argumentIndex(1)
    def source     = cpg.literal("\"XYZ\"")
    val List(flow) = sink.reachableByFlows(source).map(flowToResultPairs).l
    flow shouldBe List(("FOOBAR = \"XYZ\"", 2), ("FOOBAR = import(models, FOOBAR)", 2), ("print(FOOBAR)", 4))
  }

  "flow from global variable defined in imported file and used as argument to another module's imported method" in {
    val cpg = code("""
        |import service
        |from models import FOOBAR
        |def run():
        |   service.doThing(FOOBAR)
        |""".stripMargin)
      .moreCode(
        """
          |FOOBAR = "XYZ"
          |""".stripMargin,
        fileName = "models.py"
      )

    def sink       = cpg.call("doThing").argument.argumentIndex(1)
    def source     = cpg.literal("\"XYZ\"")
    val List(flow) = sink.reachableByFlows(source).map(flowToResultPairs).l
    flow shouldBe List(("FOOBAR = \"XYZ\"", 2), ("FOOBAR = import(models, FOOBAR)", 3), ("service.doThing(FOOBAR)", 5))
  }

  "flow from global variable defined in imported file and used as field access to `print`" in {
    val cpg = code("""
        |import models
        |print(models.FOOBAR)
        |""".stripMargin)
      .moreCode(
        """
          |FOOBAR = "XYZ"
          |""".stripMargin,
        fileName = "models.py"
      )

    def sink       = cpg.call("print").argument.argumentIndex(1)
    def source     = cpg.literal("\"XYZ\"")
    val List(flow) = sink.reachableByFlows(source).map(flowToResultPairs).l
    flow shouldBe List(("FOOBAR = \"XYZ\"", 2), ("models = import(, models)", 2), ("print(models.FOOBAR)", 3))
  }

  "flows through nested try-except structures" in {
    val cpg = code("""
        |def a9_lab(request):
        | try:
        |   file = request.FILES["file"]
        |   try:
        |     data = yaml.load(file, yaml.Loader)
        |   except:
        |     print("Failed to deserialize yaml")
        | except:
        |   print("Failed to extract file parameter from request")
        |""".stripMargin)

    // TODO: For some reason, cpg.parameter.nameExact("request").l does not work as a source
    val source = cpg.assignment.and(_.target.isIdentifier.nameExact("file"), _.source.code("request.*")).source.l
    val sink   = cpg.call.nameExact("load").argument(1).l
    sink.reachableByFlows(source).map(flowToResultPairs).l shouldBe List(
      List(("file = request.FILES[\"file\"]", 4), ("yaml.load(file, yaml.Loader)", 6))
    )
  }

}

class RegexDefinedFlowsDataFlowTests
    extends PySrc2CpgFixture(
      withOssDataflow = true,
      extraFlows = List(
        FlowSemantic.from("^path.*<module>\\.sanitizer$", List((0, 0), (1, 1)), regex = true),
        FlowSemantic.from("^foo.*<module>\\.sanitizer.*", List((0, 0), (1, 1)), regex = true),
        FlowSemantic.from("^foo.*\\.create_sanitizer\\.<returnValue>\\.sanitize", List((0, 0), (1, 1)), regex = true),
        FlowSemantic
          .from(
            "requests.py:<module>.post",
            List((0, 0), (1, "url", -1), (2, "body", -1), (1, "url", 1, "url"), (2, "body", 2, "body"))
          ),
        FlowSemantic.from("cross_taint.py:<module>.go", List((0, 0), (1, 1), (1, "a", 2, "b")))
      )
    ) {

  "regex matched semantic for imported method" should {
    lazy val cpg = code(
      """
      |from path import sanitizer
      |
      |source = 1
      |x = sanitizer(source)
      |sink(x)
      |""".stripMargin,
      Seq("foo.py").mkString(java.io.File.separator)
    )

    "register that sanitizer kills the flow on the parameter" in {
      def source = cpg.literal("1")
      def sink   = cpg.call("sink")

      sink.reachableBy(source).size shouldBe 0
    }

  }

  "regex matched semantic for more than one imported method" should {
    lazy val cpg = code(
      """
        |from foo import sanitizerFoo, sanitizerBar
        |
        |source = 1
        |x = sanitizerFoo(source)
        |y = sanitizerBar(source)
        |sink(x, y)
        |""".stripMargin,
      Seq("foo.py").mkString(java.io.File.separator)
    )

    "register that all sanitizers kill the flow on the parameter" in {
      def source = cpg.literal("1")
      def sink   = cpg.call("sink")

      sink.reachableBy(source).size shouldBe 0
    }

  }

  "regex matched semantic for a dummy type resulting from type recovery" should {
    val cpg = code(
      """
        |from foo import create_sanitizer
        |
        |source = 1
        |x = create_sanitizer()
        |y = x.sanitize(source)
        |sink(y)
        |""".stripMargin,
      Seq("foo.py").mkString(java.io.File.separator)
    )

    "register that the call off of a return value has no flow" in {
      def source = cpg.literal("1")
      def sink   = cpg.call("sink")

      sink.reachableBy(source).size shouldBe 0
    }

  }

  "flows to parameterized arguments" should {
    val cpg = code("""
        |import requests
        |def foo():
        |    orderId = "Mysource"
        |    item = orderId
        |    response = requests.post(
        |            url="https://rest.marketingcloudapis.com/data/v1/async",
        |            body=item
        |        )
        |""".stripMargin)

    "have summarized flows accurately pass parameterized argument behaviour" in {
      val source = cpg.identifier("orderId")
      val sink   = cpg.call("post")

      sink.reachableBy(source).size shouldBe 2
    }
  }

  "flows across named parameterized arguments" should {
    val cpg = code("""
        |import cross_taint
        |
        |def foo():
        |    source = "Mysource"
        |    transport = 2
        |    cross_taint.go(a=source, b=transport)
        |    sink(transport)
        |""".stripMargin)

    "have passed taint from one parameter to the next" in {
      val source = cpg.literal("\"Mysource\"")
      val sink   = cpg.call("sink")

      sink.reachableBy(source).size shouldBe 1
    }
  }

  "flows from receivers" should {
    val cpg = code("""
        |class Foo:
        |   def func():
        |      return "x"
        |print(Foo.func())
        |""".stripMargin)
    "be found" in {
      val src = cpg.call.code("Foo.func").l
      val snk = cpg.call("print").l
      snk.argument.reachableByFlows(src).size shouldBe 1
    }
  }

  "flows from receivers directly" should {
    val cpg = code("""
        |class Foo:
        |   def func():
        |      return "x"
        |print(Foo.func())
        |""".stripMargin)
    "be found" in {
      val src = cpg.identifier("Foo").l
      val snk = cpg.call("print").l
      snk.reachableByFlows(src).size shouldBe 2
    }
  }
  "Import statement with method ref sample four" in {
    val controller =
      """
        |from django.contrib import admin
        |from django.urls import path
        |from django.conf.urls import url
        |from .views import all_page
        |
        |urlpatterns = [
        |    url(r'allPage', all_page)
        |]
        |""".stripMargin
    val views =
      """
        |def all_page(request):
        |	print("All pages")
        |""".stripMargin
    val cpg = code(controller, Seq("controller", "urls.py").mkString(File.separator))
      .moreCode(views, Seq("controller", "views.py").mkString(File.separator))

    val args = cpg.call.methodFullName("django.*[.](path|url)").l.head.argument.l
    args.size shouldBe 3
  }

  "Import statement with method ref sample five" in {
    val controller =
      """
        |from django.contrib import admin
        |from django.urls import path
        |from django.conf.urls import url
        |from student.views import all_page
        |
        |urlpatterns = [
        |    url(r'allPage', all_page)
        |]
        |""".stripMargin
    val views =
      """
        |def all_page(request):
        |	print("All pages")
        |""".stripMargin
    val cpg = code(controller, Seq("controller", "urls.py").mkString(File.separator))
      .moreCode(views, Seq("student", "views.py").mkString(File.separator))

    val args = cpg.call.methodFullName("django.*[.](path|url)").l.head.argument.l
    args.size shouldBe 3
  }

  "flows via tuple literal" should {
    val cpg = code("""
        |a = 1
        |b = 2
        |c = 3
        |
        |x = (a, b, c)
        |
        |sink1(b)
        |sink2(x)
        |""".stripMargin)
    "not cross-taint due to 'pass through' semantics" in {
      val src = cpg.literal("1").l
      val snk = cpg.call("sink1").l
      snk.reachableByFlows(src).size shouldBe 0
    }

    "taint the return value due to 'pass through' semantics" in {
      val src = cpg.call.nameExact("<operator>.tupleLiteral").l
      val snk = cpg.call("sink2").l
      snk.reachableByFlows(src).size shouldBe 1
    }
  }

  "Exception block flow sample one" in {
    val cpg: Cpg = code("""
        |import logging
        |tmp = logging.getLogger(__name__)
        |
        |class AccountDetailsFetcherUtility:
        |    def getAccountDetails(cls, accountId):
        |        try:
        |            someProcessing()
        |            tmp.debug(f"Debug : {accountId}")
        |        except Exception as e:
        |            tmp.error(f"Failure: {accountId}")
        |            return None
        |""".stripMargin)
    val sources = cpg.identifier(".*account.*").lineNumber(6).l
    val sinks   = cpg.call.methodFullName(".*log.*(debug|info|error).*").l
    val flows   = sinks.reachableByFlows(sources).l
    flows.size shouldBe 2
  }

  // TODO: Need to fix this scenario. This use case is not working across the frontend. Had tested it for Java as well.
  "Exception block flow sample two" ignore {
    val cpg: Cpg = code("""
        |import logging
        |tmp = logging.getLogger(__name__)
        |
        |class AccountDetailsFetcherUtility:
        |    def getAccountDetails(cls, accountId):
        |        try:
        |            tmp.debug(f"Debug : {accountId}")
        |            return None
        |        except Exception as e:
        |            tmp.error(f"Failure: {accountId}")
        |            return None
        |""".stripMargin)
    val sources = cpg.identifier(".*account.*").lineNumber(6).l
    val sinks   = cpg.call.methodFullName(".*log.*(debug|info|error).*").l
    val flows   = sinks.reachableByFlows(sources).l
    flows.size shouldBe 2
  }

  "flow across interprocedural module variables" should {

    "handle simple import and field-based usage of a literal" in {
      val cpg: Cpg = code(
        """
          |a = 42
          |""".stripMargin,
        "foo.py"
      )
        .moreCode(
          """
            |import foo
            |
            |print(foo.a)
            |""".stripMargin,
          "bar.py"
        )
      val source = cpg.literal("42").l
      val sink   = cpg.call.name("print.*").l
      sink.reachableByFlows(source).size shouldBe 1
    }

    "handle simple import and aliased usage of a literal" in {
      val cpg: Cpg = code(
        """
          |a = 42
          |""".stripMargin,
        "foo.py"
      )
        .moreCode(
          """
            |from foo import a as b
            |
            |print(b)
            |""".stripMargin,
          "bar.py"
        )
      val source = cpg.literal("42").l
      val sink   = cpg.call.name("print.*").l
      sink.reachableByFlows(source).size shouldBe 1
    }

  }

}
