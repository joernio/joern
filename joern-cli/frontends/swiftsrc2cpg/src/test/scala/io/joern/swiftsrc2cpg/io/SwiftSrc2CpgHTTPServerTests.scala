package io.joern.swiftsrc2cpg.io

import io.joern.x2cpg.utils.server.FrontendHTTPClient
import io.shiftleft.semanticcpg.utils.FileUtil.*
import io.shiftleft.codepropertygraph.cpgloading.CpgLoader
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.utils.FileUtil
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.nio.file.{Files, Path}

import scala.collection.parallel.CollectionConverters.RangeIsParallelizable
import scala.util.Failure
import scala.util.Success

class SwiftSrc2CpgHTTPServerTests extends AnyWordSpec with Matchers with BeforeAndAfterAll {

  private var port: Int = -1

  private def newProjectUnderTest[T](index: Option[Int] = None)(f: Path => T): T = {
    FileUtil.usingTemporaryDirectory("swiftsrc2cpgTestsHttpTest") { dir =>
      val file = dir / "main.swift"
      file.createWithParentsIfNotExists(createParents = true)
      val indexStr = index.map(_.toString).getOrElse("")
      val content = s"""
                       |func main() {
                       |  println($indexStr)
                       |}""".stripMargin
      Files.writeString(file, content)
      f(dir)
    }
  }

  override def beforeAll(): Unit = {
    port = io.joern.swiftsrc2cpg.Main.server.startup()
  }

  override def afterAll(): Unit = {
    io.joern.swiftsrc2cpg.Main.server.stop()
  }

  "Using swiftsrc2cpg in server mode" should {
    "build CPGs correctly (single test)" in {
      FileUtil.usingTemporaryFile("swiftsrc2cpg", ".bin") { cpgOutFile =>
        newProjectUnderTest() { projectUnderTest =>
          val input  = projectUnderTest.toAbsolutePath.toString
          val output = cpgOutFile.toString
          val client = FrontendHTTPClient(port)
          val req    = client.buildRequest(Array(s"input=$input", s"output=$output"))
          client.sendRequest(req) match {
            case Failure(exception) => fail(exception.getMessage)
            case Success(out) =>
              out shouldBe output
              val cpg = CpgLoader.load(output)
              cpg.method.name.l should contain("main")
              cpg.call.code.l should contain("println()")
          }
        }
      }
    }

    "build CPGs correctly (multi-threaded test)" in {
      (0 until 10).par.foreach { index =>
        FileUtil.usingTemporaryFile("swiftsrc2cpg", ".bin") { cpgOutFile =>
          newProjectUnderTest(Some(index)) { projectUnderTest =>
            val input  = projectUnderTest.toAbsolutePath.toString
            val output = cpgOutFile.toString
            val client = FrontendHTTPClient(port)
            val req    = client.buildRequest(Array(s"input=$input", s"output=$output"))
            client.sendRequest(req) match {
              case Failure(exception) => fail(exception.getMessage)
              case Success(out) =>
                out shouldBe output
                val cpg = CpgLoader.load(output)
                cpg.method.name.l should contain("main")
                cpg.call.code.l should contain(s"println($index)")
            }
          }
        }
      }
    }
  }

}
