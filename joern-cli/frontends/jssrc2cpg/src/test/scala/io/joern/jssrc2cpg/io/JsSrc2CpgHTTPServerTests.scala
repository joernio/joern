package io.joern.jssrc2cpg.io

import better.files.File
import io.joern.x2cpg.utils.server.FrontendHTTPClient
import io.shiftleft.codepropertygraph.cpgloading.CpgLoader
import io.shiftleft.semanticcpg.language.*
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.util.Failure
import scala.util.Success
import scala.collection.parallel.CollectionConverters.RangeIsParallelizable

class JsSrc2CpgHTTPServerTests extends AnyWordSpec with Matchers with BeforeAndAfterAll {

  private val testPort: Int = 9002

  private def newProjectUnderTest(index: Option[Int] = None): File = {
    val dir  = File.newTemporaryDirectory("jssrc2cpgTestsHttpTest")
    val file = dir / "main.js"
    file.createIfNotExists(createParents = true)
    val indexStr = index.map(_.toString).getOrElse("")
    file.writeText(s"""
      |function main$indexStr() {
      |  console.log("Hello World!");
      |}
      |""".stripMargin)
    file.deleteOnExit()
    dir.deleteOnExit()
  }

  override def beforeAll(): Unit = {
    // Start server
    io.joern.jssrc2cpg.Main.main(Array("", "--server", s"--server-port=$testPort"))
  }

  override def afterAll(): Unit = {
    // Stop server
    io.joern.jssrc2cpg.Main.stop()
  }

  "Using jssrc2cpg in server mode" should {
    "build CPGs correctly (single test)" in {
      val cpgOutFile = File.newTemporaryFile("jssrc2cpg.bin")
      cpgOutFile.deleteOnExit()
      val projectUnderTest = newProjectUnderTest()
      val input            = projectUnderTest.path.toAbsolutePath.toString
      val output           = cpgOutFile.toString
      val client           = FrontendHTTPClient(port = testPort)
      val req              = client.buildRequest(Array(s"input=$input", s"output=$output"))
      client.sendRequest(req) match {
        case Failure(exception) => fail(exception.getMessage)
        case Success(out) =>
          out shouldBe output
          val cpg = CpgLoader.load(output)
          cpg.method.name.l should contain("main")
          cpg.call.code.l should contain("""console.log("Hello World!")""")
      }
    }

    "build CPGs correctly (multi-threaded test)" in {
      (0 until 10).par.foreach { index =>
        val cpgOutFile = File.newTemporaryFile("jssrc2cpg.bin")
        cpgOutFile.deleteOnExit()
        val projectUnderTest = newProjectUnderTest(Some(index))
        val input            = projectUnderTest.path.toAbsolutePath.toString
        val output           = cpgOutFile.toString
        val client           = FrontendHTTPClient(port = testPort)
        val req              = client.buildRequest(Array(s"input=$input", s"output=$output"))
        client.sendRequest(req) match {
          case Failure(exception) => fail(exception.getMessage)
          case Success(out) =>
            out shouldBe output
            val cpg = CpgLoader.load(output)
            cpg.method.name.l should contain(s"main$index")
            cpg.call.code.l should contain("""console.log("Hello World!")""")
        }
      }
    }
  }

}
