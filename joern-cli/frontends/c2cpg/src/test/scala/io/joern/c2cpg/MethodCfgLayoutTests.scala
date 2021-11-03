package io.joern.c2cpg

import io.joern.c2cpg.fixtures.{TestProjectFixture, TraversalUtils}
import io.shiftleft.codepropertygraph.generated.{EdgeTypes, NodeTypes, Operators, Properties}
import org.scalatest.wordspec.AnyWordSpec
import overflowdb.traversal._
import overflowdb.{Node, PropertyKey}

class MethodCfgLayoutTests extends AnyWordSpec with TraversalUtils {

  override val fixture: TestProjectFixture = TestProjectFixture("methodcfglayout")

  implicit class VertexListWrapper(vertexList: List[Node]) {
    def expandCfg(): List[Node] = {
      vertexList.flatMap(_.out(EdgeTypes.CFG).l)
    }

    def checkForSingleProperty(label: String, propertyKey: PropertyKey[String], value: String): Unit = {
      vertexList.size shouldBe 1
      vertexList.head.label shouldBe label
      vertexList.head.property(propertyKey) shouldBe value
    }

    def checkForSingle(label: String): Unit = {
      vertexList.size shouldBe 1
      vertexList.head.label shouldBe label
    }
  }

  "CFG layout" should {
    "be correct for decl assignment in method1" in {
      var result = getMethod("method1").expandCfg()
      result.checkForSingleProperty(NodeTypes.IDENTIFIER, Properties.NAME, "x")

      result = result.expandCfg()
      result.checkForSingleProperty(NodeTypes.LITERAL, Properties.CODE, "1")

      result = result.expandCfg()
      result.checkForSingleProperty(NodeTypes.CALL, Properties.NAME, Operators.assignment)

      result = result.expandCfg()
      result.checkForSingle(NodeTypes.METHOD_RETURN)
    }

    "be correct for nested expression in method2" in {
      var result = getMethod("method2").expandCfg()
      result.checkForSingleProperty(NodeTypes.IDENTIFIER, Properties.NAME, "x")

      result = result.expandCfg()
      result.checkForSingleProperty(NodeTypes.IDENTIFIER, Properties.NAME, "y")

      result = result.expandCfg()
      result.checkForSingleProperty(NodeTypes.IDENTIFIER, Properties.NAME, "z")

      result = result.expandCfg()
      result.checkForSingleProperty(NodeTypes.CALL, Properties.NAME, Operators.addition)

      result = result.expandCfg()
      result.checkForSingleProperty(NodeTypes.CALL, Properties.NAME, Operators.assignment)

      result = result.expandCfg()
      result.checkForSingle(NodeTypes.METHOD_RETURN)
    }
  }

}
