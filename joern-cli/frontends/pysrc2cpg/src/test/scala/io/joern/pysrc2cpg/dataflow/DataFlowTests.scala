package io.joern.pysrc2cpg.dataflow

import io.joern.dataflowengineoss.language.toExtendedCfgNode
import io.joern.pysrc2cpg.PySrc2CpgFixture
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.{Literal, Member}
import io.shiftleft.semanticcpg.language._

import java.io.File

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
    val sourceUrlIdentifier = cpg.identifier(".*url.*").l
    val sink                = cpg.call("post").l
    sourceUrlIdentifier.size shouldBe 2
    sink.size shouldBe 1
    sink.reachableByFlows(sourceUrlIdentifier).size shouldBe 1

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
    sink1.reachableBy(iSrc).size shouldBe 1

    val lSrc = cpg.method("foo").ast.isLiteral.code("1").lineNumber(4).l
    lSrc.size shouldBe 1
    sink1.reachableBy(lSrc).size shouldBe 1

    val pSrc = cpg.method("foo").parameter.nameExact("u").l
    pSrc.size shouldBe 1
    sink2.reachableBy(pSrc).size shouldBe 1
  }

  "Import statement with method ref sample one" in {
    val controller =
      """
        |from django.contrib import admin
        |from django.urls import path
        |from django.conf.urls import url
        |from student import views
        |
        |urlpatterns = [
        |    url(r'allPage', views.all_page)
        |]
        |""".stripMargin
    val views =
      """
        |def all_page(request):
        |	print("All pages")
        |""".stripMargin
    val cpg = code("print('Hello, world!')")
      .moreCode(controller, Seq("controller", "urls.py").mkString(File.separator))
      .moreCode(views, Seq("student", "views.py").mkString(File.separator))

    val args = cpg.call.methodFullName("django.*[.](path|url)").l.head.argument.l
    args.size shouldBe 3
  }

  "Import statement with method ref sample two" in {
    val controller =
      """
        |from django.contrib import admin
        |from django.urls import path
        |from django.conf.urls import url
        |from .import views
        |
        |urlpatterns = [
        |    url(r'allPage', views.all_page)
        |]
        |""".stripMargin
    val views =
      """
        |def all_page(request):
        |	print("All pages")
        |""".stripMargin
    val cpg = code(controller, "urls.py")
      .moreCode(views, "views.py")

    val Some(allPageRef) = cpg.call.methodFullName("django.*[.](path|url)").argument.isMethodRef.headOption
    allPageRef.methodFullName shouldBe "views.py:<module>.all_page"
    allPageRef.code shouldBe "views.all_page"
  }

  "Import statement with method ref sample three" in {
    val controller =
      """
        |from django.contrib import admin
        |from django.urls import path
        |from django.conf.urls import url
        |from .import views
        |
        |urlpatterns = [
        |    url(r'allPage', views.all_page)
        |]
        |""".stripMargin
    val views =
      """
        |def all_page(request):
        |	print("All pages")
        |""".stripMargin
    val cpg = code(controller, Seq("controller", "urls.py").mkString(File.separator))
      .moreCode(views, Seq("controller", "views.py").mkString(File.separator))

    val Some(allPageRef) = cpg.call.methodFullName("django.*[.](path|url)").argument.isMethodRef.headOption
    allPageRef.methodFullName shouldBe Seq("controller", "views.py:<module>.all_page").mkString(File.separator)
    allPageRef.code shouldBe "views.all_page"
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

    val Some(allPageRef) = cpg.call.methodFullName("django.*[.](path|url)").argument.isMethodRef.headOption
    allPageRef.methodFullName shouldBe Seq("controller", "views.py:<module>.all_page").mkString(File.separator)
    allPageRef.code shouldBe "all_page"
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

    val Some(allPageRef) = cpg.call.methodFullName("django.*[.](path|url)").argument.isMethodRef.headOption
    allPageRef.methodFullName shouldBe Seq("student", "views.py:<module>.all_page").mkString(File.separator)
    allPageRef.code shouldBe "all_page"
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
