package io.joern.pysrc2cpg.dataflow

import flatgraph.DiffGraphApplier
import io.joern.dataflowengineoss.DefaultSemantics
import io.joern.dataflowengineoss.passes.reachingdef.ReachingDefPass
import io.joern.dataflowengineoss.semanticsloader.Semantics
import io.joern.pysrc2cpg.testfixtures.PySrc2CpgFixture
import io.joern.x2cpg.frontendspecific.pysrc2cpg.PythonTypeRecoveryPassGenerator
import io.shiftleft.codepropertygraph.generated.nodes.Method
import io.shiftleft.codepropertygraph.generated.{Cpg, EdgeTypes}
import io.shiftleft.semanticcpg.language.*

/** A `METHOD_REF` is required by the schema to have a `REF` edge to its `METHOD`, and the strict accessor
  * `MethodRef.referencedMethod` throws when it does not. Passes that walk method refs should resolve the edge
  * optionally, so that a single malformed node degrades into a skipped edge rather than aborting the pass (and, for
  * `joern-export`, producing no output at all). See #5356.
  */
class MethodRefWithoutRefEdgeTests extends PySrc2CpgFixture(withOssDataflow = false) {

  /** Removes every `REF` edge leaving a `METHOD_REF`, reproducing the malformed state. */
  private def stripMethodRefRefEdges(cpg: Cpg): Unit = {
    val diffGraph = Cpg.newDiffGraphBuilder
    cpg.methodRef.foreach(_.outE(EdgeTypes.REF).foreach(diffGraph.removeEdge))
    DiffGraphApplier.applyDiff(cpg.graph, diffGraph)
  }

  "a METHOD_REF without its REF edge" should {

    "not abort ReachingDefPass" in {
      implicit val semantics: Semantics = DefaultSemantics()

      // `handler` closes over `prefix`, a parameter of the enclosing method, which is what puts a
      // parameter into `capturedByMethodRef` and thus reaches the traversal under test.
      val cpg = code("""
          |def make_handler(prefix):
          |    def handler(event):
          |        return prefix + event
          |    return handler
          |""".stripMargin).cpg

      cpg.method.name("make_handler").parameter.capturedByMethodRef.size should be > 0

      stripMethodRefRefEdges(cpg)
      cpg.methodRef.filter(_._refOut.collectAll[Method].nonEmpty).size shouldBe 0

      noException should be thrownBy new ReachingDefPass(cpg).createAndApply()
    }

    "not abort the Python type recovery" in {
      val cpg = code("""
          |class C:
          |    @classmethod
          |    def cm(cls):
          |        return cls
          |""".stripMargin).cpg

      cpg.methodRef.size should be > 0

      stripMethodRefRefEdges(cpg)

      noException should be thrownBy {
        new PythonTypeRecoveryPassGenerator(cpg).generate().foreach(_.createAndApply())
      }
    }
  }
}
