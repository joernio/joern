package io.joern.c2cpg.io

import better.files.File
import io.shiftleft.codepropertygraph.cpgloading.CpgLoader
import io.shiftleft.semanticcpg.language.*
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.net.http.HttpClient
import java.net.http.HttpClient.Redirect
import java.net.http.HttpClient.Version
import java.net.Authenticator
import java.net.http.HttpRequest
import java.net.URI
import java.net.http.HttpRequest.BodyPublishers
import java.net.http.HttpResponse.BodyHandlers
import java.time.Duration

class C2CpgHTTPServerTests extends AnyWordSpec with Matchers with BeforeAndAfterAll {

  private val testPort: Int = 9001

  private val client: HttpClient = HttpClient.newBuilder().connectTimeout(Duration.ofSeconds(5)).build()

  private def buildRequest(args: Array[String]): HttpRequest = {
    HttpRequest
      .newBuilder()
      .uri(URI.create(s"http://localhost:$testPort/run"))
      .timeout(Duration.ofSeconds(5))
      .header("Content-Type", "application/x-www-form-urlencoded")
      .POST(BodyPublishers.ofString(args.mkString("&")))
      .build()
  }

  private def sendRequest(req: HttpRequest): String = {
    val resp = client.send(req, BodyHandlers.ofString())
    resp match {
      case r if r.statusCode() == 200 => resp.body()
      case r                          => fail(s"Sending request failed with code ${r.statusCode()}: ${r.body()}")
    }
  }

  private val projectUnderTest: File = {
    val dir  = File.newTemporaryDirectory("c2cpgTestsExcludeTest")
    val file = dir / "main.c"
    file.createIfNotExists(createParents = true)
    file.writeText("""
        |int main(int argc, char *argv[]) {
        |  print("Hello World!");
        |}
        |""".stripMargin)
    dir
  }

  override def beforeAll(): Unit = {
    // Start server
    io.joern.c2cpg.Main.main(Array("", "--server", s"--server-port=$testPort"))
  }

  override def afterAll(): Unit = {
    // Stop server
    io.joern.c2cpg.Main.stop()
    projectUnderTest.delete(swallowIOExceptions = true)
  }

  "Using c2cpg in server mode" should {
    "build CPGs correctly" in {
      val cpgOutFile = File.newTemporaryFile("c2cpg.bin")
      cpgOutFile.deleteOnExit()
      val input      = projectUnderTest.path.toAbsolutePath.toString
      val output     = cpgOutFile.toString
      val req        = buildRequest(Array(s"input=$input", s"output=$output"))
      val respOutput = sendRequest(req)

      respOutput shouldBe output
      val cpg = CpgLoader.load(output)
      cpg.method.name.l should contain("main")
      cpg.call.code.l shouldBe List("""print("Hello World!")""")
    }
  }

}
