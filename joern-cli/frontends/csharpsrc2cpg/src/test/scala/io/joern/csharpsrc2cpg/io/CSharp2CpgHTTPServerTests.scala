package io.joern.csharpsrc2cpg.io

import better.files.File
import io.joern.csharpsrc2cpg.testfixtures.CSharpCode2CpgFixture
import io.joern.x2cpg.utils.server.FrontendHTTPClient
import io.shiftleft.codepropertygraph.cpgloading.CpgLoader
import io.shiftleft.semanticcpg.language.*
import org.scalatest.BeforeAndAfterAll

import scala.collection.parallel.CollectionConverters.RangeIsParallelizable
import scala.util.Failure
import scala.util.Success

class CSharp2CpgHTTPServerTests extends CSharpCode2CpgFixture with BeforeAndAfterAll {

  private var port: Int = -1

  private def newProjectUnderTest(index: Option[Int] = None): File = {
    val dir  = File.newTemporaryDirectory("csharp2cpgTestsHttpTest")
    val file = dir / "main.cs"
    file.createIfNotExists(createParents = true)
    val indexStr = index.map(_.toString).getOrElse("")
    file.writeText(basicBoilerplate(s"Console.WriteLine($indexStr);"))
    file.deleteOnExit()
    dir.deleteOnExit()
  }

  override def beforeAll(): Unit = {
    // Start server
    port = io.joern.csharpsrc2cpg.Main.startup()
  }

  override def afterAll(): Unit = {
    // Stop server
    io.joern.csharpsrc2cpg.Main.stop()
  }

  "Using csharp2cpg in server mode" should {
    "build CPGs correctly (single test)" in {
      val cpgOutFile = File.newTemporaryFile("csharp2cpg.bin")
      cpgOutFile.deleteOnExit()
      val projectUnderTest = newProjectUnderTest()
      val input            = projectUnderTest.path.toAbsolutePath.toString
      val output           = cpgOutFile.toString
      val client           = FrontendHTTPClient(port)
      val req              = client.buildRequest(Array(s"input=$input", s"output=$output"))
      client.sendRequest(req) match {
        case Failure(exception) => fail(exception.getMessage)
        case Success(out) =>
          out shouldBe output
          val cpg = CpgLoader.load(output)
          cpg.method.name.l should contain("Main")
          cpg.call.code.l shouldBe List("Console.WriteLine()")
      }
    }

    "build CPGs correctly (multi-threaded test)" in {
      (0 until 10).par.foreach { index =>
        val cpgOutFile = File.newTemporaryFile("csharp2cpg.bin")
        cpgOutFile.deleteOnExit()
        val projectUnderTest = newProjectUnderTest(Some(index))
        val input            = projectUnderTest.path.toAbsolutePath.toString
        val output           = cpgOutFile.toString
        val client           = FrontendHTTPClient(port)
        val req              = client.buildRequest(Array(s"input=$input", s"output=$output"))
        client.sendRequest(req) match {
          case Failure(exception) => fail(exception.getMessage)
          case Success(out) =>
            out shouldBe output
            val cpg = CpgLoader.load(output)
            cpg.method.name.l should contain("Main")
            cpg.call.code.l shouldBe List(s"Console.WriteLine($index)")
        }
      }
    }
  }

}
