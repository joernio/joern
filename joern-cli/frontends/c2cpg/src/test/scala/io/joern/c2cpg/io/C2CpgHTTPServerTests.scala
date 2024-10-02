package io.joern.c2cpg.io

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

class C2CpgHTTPServerTests extends AnyWordSpec with Matchers with BeforeAndAfterAll {

  private var port: Int = -1

  private def newProjectUnderTest(index: Option[Int] = None): File = {
    val dir  = File.newTemporaryDirectory("c2cpgTestsHttpTest")
    val file = dir / "main.c"
    file.createIfNotExists(createParents = true)
    val indexStr = index.map(_.toString).getOrElse("")
    file.writeText(s"""
        |int main$indexStr(int argc, char *argv[]) {
        |  print("Hello World!");
        |}
        |""".stripMargin)
    file.deleteOnExit()
    dir.deleteOnExit()
  }

  override def beforeAll(): Unit = {
    // Start server
    port = io.joern.c2cpg.Main.startup()
  }

  override def afterAll(): Unit = {
    // Stop server
    io.joern.c2cpg.Main.stop()
  }

  "Using c2cpg in server mode" should {
    "build CPGs correctly (single test)" in {
      val cpgOutFile = File.newTemporaryFile("c2cpg.bin")
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
          cpg.method.name.l should contain("main")
          cpg.call.code.l shouldBe List("""print("Hello World!")""")
      }
    }

    "build CPGs correctly (multi-threaded test)" in {
      (0 until 10).par.foreach { index =>
        val cpgOutFile = File.newTemporaryFile("c2cpg.bin")
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
            cpg.method.name.l should contain(s"main$index")
            cpg.call.code.l shouldBe List("""print("Hello World!")""")
        }
      }
    }
  }

}
