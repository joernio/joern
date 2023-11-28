package io.joern.x2cpg.testfixtures

import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach, Inside}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.collection.mutable

// Fixture class from which all tests which require a code to CPG translation step
// should either directly or indirectly use. The intended way is to derive from
// this class, thereby specifying the testCpgFactory parameter and than use the
// derived class in tests.
// The testCpgFactory() and code() methods return a type T deriving from TestCpg
// in order to allow further cpg and language frontend configuration methods to
// be defined as needed for the individual test cases.
class Code2CpgFixture[T <: TestCpg](testCpgFactory: () => T)
    extends AnyWordSpec
    with Matchers
    with BeforeAndAfterAll
    with BeforeAndAfterEach
    with Inside {
  private val cpgs = mutable.ArrayBuffer.empty[TestCpg]

  def code(code: String): T = {
    val newCpg = testCpgFactory().moreCode(code)
    cpgs.append(newCpg)
    newCpg
  }

  def code(code: String, fileName: String): T = {
    val newCpg = testCpgFactory().moreCode(code, fileName)
    cpgs.append(newCpg)
    newCpg
  }

  override def afterAll(): Unit = {
    cpgs.foreach(_.close())
  }
}
