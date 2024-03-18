package io.joern.javasrc2cpg.testfixtures

import io.joern.x2cpg.utils.TestCodeWriter
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.collection.mutable

class SourceCodeFixture extends AnyWordSpec with Matchers with BeforeAndAfterAll {
  private class TestCodeWriterImpl extends TestCodeWriter

  val writers = mutable.ListBuffer.empty[TestCodeWriter]

  override def afterAll(): Unit = {
    writers.foreach(_.cleanupOutput())
    super.afterAll()
  }
  def code(code: String, fileName: String): TestCodeWriter = {
    emptyWriter.moreCode(code, fileName)

  }

  def code(code: String): TestCodeWriter = {
    emptyWriter.moreCode(code)
  }

  def emptyWriter: TestCodeWriter = {
    val writer = new TestCodeWriterImpl()
    writers.addOne(writer)
    writer
  }
}
