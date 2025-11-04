package io.joern.swiftsrc2cpg.passes.config

import com.dd.plist.PropertyListConverter
import flatgraph.DNode
import io.joern.swiftsrc2cpg.Config
import io.joern.swiftsrc2cpg.passes.ConfigFileCreationPass
import io.shiftleft.codepropertygraph.generated.GraphSchema
import io.shiftleft.codepropertygraph.generated.nodes.NewConfigFile
import io.shiftleft.semanticcpg.utils.FileUtil
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.Files

class ConfigFileCreationPassTests extends AnyFunSpec with Matchers {

  private class TestDiffGraphBuilder extends io.shiftleft.codepropertygraph.generated.DiffGraphBuilder(GraphSchema) {
    val nodes = scala.collection.mutable.Buffer[DNode]()
    override def addNode(newNode: DNode): this.type = {
      this.nodes.append(newNode)
      this
    }
  }

  private val Xml: String =
    """<?xml version="1.0" encoding="UTF-8"?>
      |<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
      |<plist version="1.0">
      |<dict>
      |  <key>Example</key>
      |  <string>value</string>
      |</dict>
      |</plist>""".stripMargin
      // com.dd.plist.NSObject#toXMLPropertyList uses line separator when converting
      .replace("\n", System.lineSeparator())
      // com.dd.plist.NSObject#toXMLPropertyList uses tabs when converting
      .replace("  ", "\t")

  describe("ConfigFileCreationPass") {
    it("creates config node for an XML plist") {
      FileUtil.usingTemporaryFile("test", ".plist") { xmlFile =>
        Files.write(xmlFile, Xml.getBytes("UTF-8"))

        val pass = new ConfigFileCreationPass(null /* cpg not used in here */, Config())
        val diff = new TestDiffGraphBuilder

        pass.runOnPart(diff, xmlFile)
        diff.nodes.nonEmpty shouldBe true
        val node = diff.nodes.collectFirst { case n: NewConfigFile => n }.get
        node.name should (startWith("test") and endWith(".plist"))
        node.content shouldBe Xml
      }
    }

    it("creates config node for a binary plist") {
      FileUtil.usingTemporaryFile("testbin-src", ".plist") { xmlFile =>
        FileUtil.usingTemporaryFile("testbin", ".plist") { binFile =>
          Files.write(xmlFile, Xml.getBytes("UTF-8"))
          PropertyListConverter.convertToBinary(xmlFile, binFile)

          val pass = new ConfigFileCreationPass(null /* cpg not used in here */, Config())
          val diff = new TestDiffGraphBuilder

          pass.runOnPart(diff, binFile)

          diff.nodes.nonEmpty shouldBe true
          val node = diff.nodes.collectFirst { case n: NewConfigFile => n }.get
          node.name should (startWith("testbin") and endWith(".plist"))
          // binary should have been converted to XML content:
          node.content should startWith(Xml)
          // and the content should have a comment referencing the original file at the very end
          node.content.linesIterator.toList.last should startWith("<!--This file was generated from")
        }
      }
    }
  }
}
