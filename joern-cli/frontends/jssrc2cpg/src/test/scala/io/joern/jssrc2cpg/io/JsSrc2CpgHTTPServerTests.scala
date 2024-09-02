package io.joern.jssrc2cpg.io

import better.files.File
import io.joern.x2cpg.utils.server.FrontendHTTPClient
import io.shiftleft.codepropertygraph.cpgloading.CpgLoader
import io.shiftleft.semanticcpg.language.*
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.time.Duration
import scala.util.Failure
import scala.util.Success

class JsSrc2CpgHTTPServerTests extends AnyWordSpec with Matchers with BeforeAndAfterAll {

  private val testTimeout   = Duration.ofSeconds(30) // astgen is a bit slow on some Github runners
  private val testPort: Int = 9002

  private val projectUnderTest: File = {
    val dir  = File.newTemporaryDirectory("jssrc2cpgTestsHttpTest")
    val file = dir / "main.js"
    file.createIfNotExists(createParents = true)
    file.writeText("""
     |function main() {
     |  console.log("Hello World!");
     |}
     |""".stripMargin)
    dir
  }

  override def beforeAll(): Unit = {
    // Start server
    io.joern.jssrc2cpg.Main.main(Array("", "--server", s"--server-port=$testPort"))
  }

  override def afterAll(): Unit = {
    // Stop server
    io.joern.jssrc2cpg.Main.stop()
    projectUnderTest.delete(swallowIOExceptions = true)
  }

  "Using jssrc2cpg in server mode" should {
    "build CPGs correctly" in {
      val cpgOutFile = File.newTemporaryFile("jssrc2cpg.bin")
      cpgOutFile.deleteOnExit()
      val input  = projectUnderTest.path.toAbsolutePath.toString
      val output = cpgOutFile.toString
      val client = FrontendHTTPClient(port = testPort, timeout = testTimeout)
      val req    = client.buildRequest(Array(s"input=$input", s"output=$output"))
      client.sendRequest(req) match {
        case Failure(exception) => fail(exception.getMessage)
        case Success(out) =>
          out shouldBe output
          val cpg = CpgLoader.load(output)
          cpg.method.name.l should contain("main")
          cpg.call.code.l should contain("""console.log("Hello World!")""")
      }
    }
  }

}
