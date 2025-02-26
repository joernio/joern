package io.joern.jimple2cpg.io

import better.files.File as BetterFile
import io.joern.jimple2cpg.testfixtures.JimpleCode2CpgFixture
import io.joern.jimple2cpg.testfixtures.JimpleCodeToCpgFixture
import io.joern.x2cpg.utils.server.FrontendHTTPClient
import io.joern.x2cpg.utils.FileUtil
import io.joern.x2cpg.utils.FileUtil.*
import io.shiftleft.codepropertygraph.cpgloading.CpgLoader
import io.shiftleft.semanticcpg.language.*
import org.scalatest.BeforeAndAfterAll

import java.io.File
import java.nio.file.Files

import scala.collection.parallel.CollectionConverters.RangeIsParallelizable
import scala.util.Failure
import scala.util.Success

class Jimple2CpgHTTPServerTests extends JimpleCode2CpgFixture with BeforeAndAfterAll {

  private var port: Int = -1

  private def newProjectUnderTest(index: Option[Int] = None): File = {
    val dir  = FileUtil.newTemporaryDirectory("jimple2cpgTestsHttpTest")
    val file = dir / "main.java"
    file.createIfNotExists(createParents = true)
    val indexStr = index.map(_.toString).getOrElse("")
    val content = s"""
                     |class Foo {
                     |  static void main$indexStr(int argc, char argv) {
                     |    System.out.println("Hello World!");
                     |  }
                     |}
                     |""".stripMargin
    Files.writeString(file.toPath, content)
    JimpleCodeToCpgFixture.compileJava(dir.toPath, List(file))
    file.deleteOnExit()
    dir.deleteOnExit()
    dir
  }

  override def beforeAll(): Unit = {
    // Start server
    port = io.joern.jimple2cpg.Main.startup()
  }

  override def afterAll(): Unit = {
    // Stop server
    io.joern.jimple2cpg.Main.stop()
  }

  "Using jimple2cpg in server mode" should {
    "build CPGs correctly (single test)" in {
      val cpgOutFile = BetterFile.newTemporaryFile("jimple2cpg.bin")
      cpgOutFile.deleteOnExit()
      val projectUnderTest = newProjectUnderTest()
      val input            = projectUnderTest.toPath.toAbsolutePath.toString
      val output           = cpgOutFile.toString
      val client           = FrontendHTTPClient(port)
      val req              = client.buildRequest(Array(s"input=$input", s"output=$output"))
      client.sendRequest(req) match {
        case Failure(exception) => fail(exception.getMessage)
        case Success(out) =>
          out shouldBe output
          val cpg = CpgLoader.load(output)
          cpg.method.name.l should contain("main")
          cpg.call.code.l should contain("""$stack2.println("Hello World!")""")
      }
    }

    "build CPGs correctly (multi-threaded test)" in {
      (0 until 10).par.foreach { index =>
        val cpgOutFile = BetterFile.newTemporaryFile("jimple2cpg.bin")
        cpgOutFile.deleteOnExit()
        val projectUnderTest = newProjectUnderTest(Some(index))
        val input            = projectUnderTest.toPath.toAbsolutePath.toString
        val output           = cpgOutFile.toString
        val client           = FrontendHTTPClient(port)
        val req              = client.buildRequest(Array(s"input=$input", s"output=$output"))
        client.sendRequest(req) match {
          case Failure(exception) => fail(exception.getMessage)
          case Success(out) =>
            out shouldBe output
            val cpg = CpgLoader.load(output)
            cpg.method.name.l should contain(s"main$index")
            cpg.call.code.l should contain("""$stack2.println("Hello World!")""")
        }
      }
    }
  }

}
