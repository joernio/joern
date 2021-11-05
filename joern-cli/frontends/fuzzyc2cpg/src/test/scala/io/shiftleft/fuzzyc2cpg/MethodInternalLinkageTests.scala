package io.shiftleft.fuzzyc2cpg

import io.shiftleft.codepropertygraph.generated.{EdgeTypes, NodeTypes, Properties}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import overflowdb._
import overflowdb.traversal._

class MethodInternalLinkageTests extends AnyWordSpec with Matchers with TraversalUtils {
  val fixture = CpgTestFixture("methodinternallinkage")

  implicit class VertexListWrapper(vertexList: List[Node]) {
    def expandAst(filterLabels: String*): List[Node] = {
      if (filterLabels.nonEmpty) {
        vertexList.flatMap(_.out(EdgeTypes.AST).hasLabel(filterLabels: _*).l)
      } else {
        vertexList.flatMap(_.out(EdgeTypes.AST).l)
      }
    }

    def expandRef(): List[Node] = {
      vertexList.flatMap(_.out(EdgeTypes.REF).l)
    }

    def filterOrder(order: Int): List[Node] = {
      vertexList.to(Traversal).has(Properties.ORDER -> order).l
    }

    def filterName(name: String): List[Node] = {
      vertexList.to(Traversal).has(Properties.NAME -> name).l
    }

    def checkForSingle[T](label: String, propertyKey: PropertyKey[T], value: T): Unit = {
      vertexList.size shouldBe 1
      vertexList.head.label() shouldBe label
      vertexList.head.property(propertyKey) shouldBe value
    }

    def checkForSingle[T](propertyKey: PropertyKey[T], value: T): Unit = {
      vertexList.size shouldBe 1
      vertexList.head.property(propertyKey) shouldBe value
    }
  }

  "REF edges" should {
    "be correct for local x in method1" in {
      val method = getMethod("method1")
      val indentifierX = method.expandAst().expandAst().expandAst(NodeTypes.IDENTIFIER)
      indentifierX.checkForSingle(Properties.NAME, "x")

      val localX = indentifierX.expandRef()
      localX.checkForSingle(NodeTypes.LOCAL, Properties.NAME, "x")
    }

    "be correct for parameter x in method2" in {
      val method = getMethod("method2")
      val indentifierX = method.expandAst().expandAst().expandAst(NodeTypes.IDENTIFIER)
      indentifierX.checkForSingle(Properties.NAME, "x")

      val parameterX = indentifierX.expandRef()
      parameterX.checkForSingle(NodeTypes.METHOD_PARAMETER_IN, Properties.NAME, "x")
    }

    "be correct for all identifiers x, y in method3" in {
      val method = getMethod("method3")
      val outerIdentifierX = method.expandAst().expandAst().filterOrder(3).expandAst(NodeTypes.IDENTIFIER)
      outerIdentifierX.checkForSingle(Properties.NAME, "x")
      val parameterX = outerIdentifierX.expandRef()
      parameterX.checkForSingle(NodeTypes.METHOD_PARAMETER_IN, Properties.NAME, "x")
      val expectedParameterX = method.expandAst(NodeTypes.METHOD_PARAMETER_IN)
      expectedParameterX.checkForSingle(Properties.NAME, "x")
      parameterX shouldBe expectedParameterX

      val outerIdentifierY = method.expandAst().expandAst().filterOrder(4).expandAst(NodeTypes.IDENTIFIER)
      outerIdentifierY.checkForSingle(Properties.NAME, "y")
      val outerLocalY = outerIdentifierY.expandRef()
      outerLocalY.checkForSingle(NodeTypes.LOCAL, Properties.NAME, "y")
      val expectedOuterLocalY = method.expandAst().expandAst(NodeTypes.LOCAL)
      expectedOuterLocalY.checkForSingle(Properties.NAME, "y")
      outerLocalY shouldBe expectedOuterLocalY

      val nestedBlock = method.expandAst().expandAst(NodeTypes.BLOCK)

      val nestedIdentifierX = nestedBlock.expandAst().filterOrder(3).expandAst(NodeTypes.IDENTIFIER)
      nestedIdentifierX.checkForSingle(Properties.NAME, "x")
      val nestedLocalX = nestedIdentifierX.expandRef()
      nestedLocalX.checkForSingle(NodeTypes.LOCAL, Properties.NAME, "x")
      val expectedNestedLocalX = nestedBlock.expandAst(NodeTypes.LOCAL).filterName("x")
      expectedNestedLocalX.checkForSingle(Properties.NAME, "x")
      nestedLocalX shouldBe expectedNestedLocalX

      val nestedIdentifierY = nestedBlock.expandAst().filterOrder(4).expandAst(NodeTypes.IDENTIFIER)
      nestedIdentifierY.checkForSingle(Properties.NAME, "y")
      val nestedLocalY = nestedIdentifierY.expandRef()
      nestedLocalY.checkForSingle(NodeTypes.LOCAL, Properties.NAME, "y")
      val expectedNestedLocalY = nestedBlock.expandAst(NodeTypes.LOCAL).filterName("y")
      expectedNestedLocalY.checkForSingle(Properties.NAME, "y")
      nestedLocalY shouldBe expectedNestedLocalY
    }
  }

}
