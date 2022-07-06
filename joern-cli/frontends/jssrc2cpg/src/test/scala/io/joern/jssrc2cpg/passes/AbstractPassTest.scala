package io.joern.jssrc2cpg.passes

import better.files.File
import io.joern.jssrc2cpg.testfixtures.JsSrc2CpgFrontend
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.{EdgeTypes, NodeTypes}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.Inside
import overflowdb.Node
import overflowdb.traversal._

import scala.jdk.CollectionConverters._

abstract class AbstractPassTest extends AnyWordSpec with Matchers with Inside {

  protected abstract class Fixture

  protected object AstFixture extends Fixture {
    def apply(code: String, filename: String = "code.js")(f: Cpg => Unit): Unit = {
      File.usingTemporaryDirectory("jssrc2cpgTest") { dir =>
        val file = dir / filename
        file.write(code)
        file.deleteOnExit()
        val cpg = new JsSrc2CpgFrontend().execute(dir.toJava)
        f(cpg)
      }
    }

    def apply(testFile: File)(f: Cpg => Unit): Unit = {
      val file = testFile
      val cpg  = new JsSrc2CpgFrontend().execute(file.parent.toJava)
      f(cpg)
    }
  }

  protected def getDependencies(cpg: Cpg): Traversal[Node] = {
    TraversalSource(cpg.graph).label(NodeTypes.DEPENDENCY)
  }

  protected def getImports(cpg: Cpg): Traversal[Node] = {
    TraversalSource(cpg.graph).label(NodeTypes.IMPORT)
  }

  protected implicit class NodeWrapper[T <: Node](nodes: Traversal[T]) {
    def expandAst(filterLabels: String*): Traversal[T] = {
      expand(EdgeTypes.AST, filterLabels: _*)
    }

    def expandRef(filterLabels: String*): Traversal[T] = {
      expand(EdgeTypes.REF, filterLabels: _*)
    }

    def expandCapture(filterLabels: String*): Traversal[T] = {
      expand(EdgeTypes.CAPTURE, filterLabels: _*)
    }

    def expandReceiver(filterLabels: String*): Traversal[T] = {
      expand(EdgeTypes.RECEIVER, filterLabels: _*)
    }

    def expand(edgeKind: String, filterLabels: String*): Traversal[T] = {
      nodes
        .outE(edgeKind)
        .collect {
          case e if filterLabels.isEmpty || filterLabels.contains(e.inNode().label()) =>
            e.inNode().asInstanceOf[T]
        }
    }

    def checkNodeCount(count: Int): Traversal[T] = {
      nodes.size shouldBe count
      nodes
    }

    def checkProperty[P](nodeProperty: String, value: P): Traversal[T] = {
      nodes.foreach { node =>
        node.property(nodeProperty) shouldBe value
      }
      nodes
    }

    def filter[P](nodeProperty: String, value: P): Traversal[T] = {
      nodes.filter { node =>
        node.propertiesMap.asScala.exists { case (property, propertyValue) =>
          property == nodeProperty && propertyValue == value
        }
      }
    }
  }

}
