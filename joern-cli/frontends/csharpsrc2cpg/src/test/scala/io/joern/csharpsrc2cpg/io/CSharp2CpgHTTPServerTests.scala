package io.joern.csharpsrc2cpg.io

import io.joern.csharpsrc2cpg.testfixtures.CSharpCode2CpgFixture
import io.joern.x2cpg.utils.server.FrontendHTTPClient
import io.shiftleft.semanticcpg.utils.FileUtil.*
import io.shiftleft.codepropertygraph.cpgloading.CpgLoader
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.utils.FileUtil
import org.scalatest.BeforeAndAfterAll

import java.nio.file.{Files, Path}

import scala.collection.parallel.CollectionConverters.RangeIsParallelizable
import scala.util.Failure
import scala.util.Success

class CSharp2CpgHTTPServerTests extends CSharpCode2CpgFixture with BeforeAndAfterAll {

  private var port: Int = -1

  private def usingProjectUnderTest[T](index: Option[Int] = None)(f: Path => T): T = {
    FileUtil.usingTemporaryDirectory("csharp2cpgTestsHttpTest") { dir =>
      val file = dir / "main.cs"
      file.createWithParentsIfNotExists(createParents = true)
      val indexStr = index.map(_.toString).getOrElse("")
      val content  = basicBoilerplate(s"Console.WriteLine($indexStr);")
      Files.writeString(file, content)
      f(dir)
    }
  }

  override def beforeAll(): Unit = {
    port = io.joern.csharpsrc2cpg.Main.server.startup()
  }

  override def afterAll(): Unit = {
    io.joern.csharpsrc2cpg.Main.server.stop()
  }

  "Using csharp2cpg in server mode" should {
    "build CPGs correctly (single test)" in {
      FileUtil.usingTemporaryFile(suffix = "csharp2cpg.bin") { cpgOutFile =>
        usingProjectUnderTest() { projectUnderTest =>
          val input  = projectUnderTest.toAbsolutePath.toString
          val output = cpgOutFile.toString
          val client = FrontendHTTPClient(port)
          val req    = client.buildRequest(Array(s"input=$input", s"output=$output"))
          client.sendRequest(req) match {
            case Failure(exception) => fail(exception.getMessage)
            case Success(out) =>
              out shouldBe output
              val cpg = CpgLoader.load(output)
              cpg.method.name.l should contain("Main")
              cpg.call.code.l shouldBe List("Console.WriteLine()")
          }
        }
      }
    }

    "build CPGs correctly (multi-threaded test)" in {
      (0 until 10).par.foreach { index =>
        FileUtil.usingTemporaryFile(suffix = "csharp2cpg.bin") { cpgOutFile =>
          usingProjectUnderTest(Some(index)) { projectUnderTest =>
            val input  = projectUnderTest.toAbsolutePath.toString
            val output = cpgOutFile.toString
            val client = FrontendHTTPClient(port)
            val req    = client.buildRequest(Array(s"input=$input", s"output=$output"))
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
  }

}
