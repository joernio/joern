package io.joern.joerncli

import flatgraph.misc.TestUtils.{addNode, applyDiff}
import io.joern.console.FrontendConfig
import io.joern.console.cpgcreation.PythonSrcCpgGenerator
import io.joern.dataflowengineoss.layers.dataflows.OssDataFlow
import io.joern.joerncli.JoernParse.ParserConfig
import io.joern.x2cpg.layers.Base
import io.joern.x2cpg.passes.frontend.MetaDataPass
import io.shiftleft.codepropertygraph.generated.nodes.{
  NewBlock,
  NewFile,
  NewMethod,
  NewMethodReturn,
  NewNamespaceBlock,
  NewType,
  NewTypeDecl
}
import io.shiftleft.codepropertygraph.generated.{Cpg, EdgeTypes, Languages}
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.utils.FileUtil
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.Inside.inside

import scala.util.{Failure, Success}

class JoernParseTests extends AnyWordSpec with Matchers {

  "joern-parse --overlaysonly" should {
    // Regression test for https://github.com/joernio/joern/issues/6283: no frontend runs in
    // --overlaysonly mode, so the post-processing generator must be resolved from the CPG itself.
    "apply default overlays to an existing CPG without running a frontend" in {
      FileUtil.usingTemporaryDirectory("joern-parse-test") { tmpDir =>
        val cpgPath = tmpDir.resolve("cpg.bin")
        val cpg     = Cpg.withStorage(cpgPath)
        new MetaDataPass(cpg, Languages.PHP, tmpDir.toString).createAndApply()
        cpg.close()

        val config = ParserConfig(
          inputPath = cpgPath.toString,
          outputCpgFile = cpgPath.toString,
          language = "php",
          enhanceOnly = true
        )

        JoernParse.run(config) match {
          case Failure(exception) => fail("joern-parse --overlaysonly failed", exception)
          case Success(_)         =>
            val enhancedCpg = CpgBasedTool.loadFromFile(cpgPath.toString)
            try {
              enhancedCpg.metaData.overlays.l should contain.allOf(Base.overlayName, OssDataFlow.overlayName)
            } finally {
              enhancedCpg.close()
            }
        }
      }
    }

    "fail with an error if the CPG has no metadata node" in {
      FileUtil.usingTemporaryDirectory("joern-parse-test") { tmpDir =>
        val cpgPath = tmpDir.resolve("cpg.bin")
        // A well-formed CPG apart from the missing MetaData node
        val cpg   = Cpg.withStorage(cpgPath)
        val graph = cpg.graph
        val file  = graph.addNode(NewFile().name("foo.php"))
        val td    = graph.addNode(NewTypeDecl().name("foo").fullName("foo"))
        val m     = graph.addNode(NewMethod().name("m").fullName("m"))
        val block = graph.addNode(NewBlock())
        val ret   = graph.addNode(NewMethodReturn())
        graph.applyDiff { d =>
          d.addEdge(file, td, EdgeTypes.AST)
          d.addEdge(td, m, EdgeTypes.AST)
          d.addEdge(m, block, EdgeTypes.AST)
          d.addEdge(m, ret, EdgeTypes.AST)
        }
        cpg.close()

        val config = ParserConfig(
          inputPath = cpgPath.toString,
          outputCpgFile = cpgPath.toString,
          language = "php",
          enhanceOnly = true
        )

        inside(JoernParse.run(config)) { case Failure(exception) =>
          exception.getMessage should include("no metadata node")
        }
      }
    }
  }

  "importing a CPG that joern-parse already post-processed" should {
    // Regression test for https://github.com/joernio/joern/issues/4830: the Python inheritance pass
    // mangled the base names it had already resolved when it ran a second time.
    "not run the Python post-processing passes again" in {
      FileUtil.usingTemporaryDirectory("joern-parse-test") { tmpDir =>
        val cpgPath = tmpDir.resolve("cpg.bin")
        val cpg     = Cpg.withStorage(cpgPath)
        new MetaDataPass(cpg, Languages.PYTHONSRC, tmpDir.toString).createAndApply()
        val graph     = cpg.graph
        val file      = graph.addNode(NewFile().name("foo.py"))
        val namespace =
          graph.addNode(NewNamespaceBlock().name("<global>").fullName("foo.py:<module>").filename("foo.py"))
        val foo = graph.addNode(
          NewTypeDecl()
            .name("Foo")
            .fullName("foo.py:<module>.Foo")
            .filename("foo.py")
            .inheritsFromTypeFullName(Seq("object"))
        )
        val bar = graph.addNode(
          NewTypeDecl()
            .name("Bar")
            .fullName("foo.py:<module>.Bar")
            .filename("foo.py")
            .inheritsFromTypeFullName(Seq("Foo"))
        )
        graph.addNode(NewType().name("Foo").fullName("foo.py:<module>.Foo").typeDeclFullName("foo.py:<module>.Foo"))
        graph.addNode(NewType().name("Bar").fullName("foo.py:<module>.Bar").typeDeclFullName("foo.py:<module>.Bar"))
        graph.applyDiff { diffGraph =>
          diffGraph.addEdge(file, namespace, EdgeTypes.AST)
          diffGraph.addEdge(namespace, foo, EdgeTypes.AST)
          diffGraph.addEdge(namespace, bar, EdgeTypes.AST)
        }
        cpg.close()

        val config = ParserConfig(
          inputPath = cpgPath.toString,
          outputCpgFile = cpgPath.toString,
          language = "pythonsrc",
          enhanceOnly = true
        )
        JoernParse.run(config) shouldBe a[Success[?]]

        def inheritance(cpg: Cpg) =
          cpg.typeDecl
            .isExternal(false)
            .filterNot(_.name.startsWith("<"))
            .filterNot(_.name.endsWith("<meta>"))
            .l
            .sortBy(_.fullName)
            .map(typeDecl => typeDecl.fullName -> typeDecl.inheritsFromTypeFullName.l)
        def externalTypeDecls(cpg: Cpg) = cpg.typeDecl.isExternal(true).fullName.l.sorted

        val importedCpg = CpgBasedTool.loadFromFile(cpgPath.toString)
        try {
          val expected =
            List("foo.py:<module>.Bar" -> List("foo.py:<module>.Foo"), "foo.py:<module>.Foo" -> List("object"))
          inheritance(importedCpg) shouldBe expected
          val externalBefore = externalTypeDecls(importedCpg)

          // what `importCpg` does after loading the CPG
          PythonSrcCpgGenerator(FrontendConfig(), tmpDir).applyPostProcessingPasses(importedCpg)

          inheritance(importedCpg) shouldBe expected
          externalTypeDecls(importedCpg) shouldBe externalBefore
          importedCpg.typeDecl.nameExact("Bar").baseType.fullName.l shouldBe List("foo.py:<module>.Foo")
        } finally {
          importedCpg.close()
        }
      }
    }
  }
}
