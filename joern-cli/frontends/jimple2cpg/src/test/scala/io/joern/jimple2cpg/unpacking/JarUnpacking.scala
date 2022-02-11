package io.joern.jimple2cpg.unpacking

import io.joern.jimple2cpg.Jimple2Cpg
import io.joern.jimple2cpg.util.ProgramHandlingUtil
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.semanticcpg.language._
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.must.Matchers
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import org.scalatest.wordspec.AnyWordSpec

import scala.reflect.io.File
import scala.util.{Failure, Success, Try}

class JarUnpacking extends AnyWordSpec with Matchers with BeforeAndAfterAll {

  var cpg: Cpg = _

  override protected def beforeAll(): Unit = {
    super.beforeAll()
    Try(getClass.getResource("/unpacking")) match {
      case Success(x) =>
        cpg = new Jimple2Cpg().createCpg(x.getPath)
      case Failure(x: Throwable) =>
        fail("Unable to obtain test resources.", x)
    }
  }

  "should extract files and clean up temp directory" in {
    Try(getClass.getResource("/unpacking")) match {
      case Success(x) =>
        val fs = File(x.getPath).toDirectory.walk.toSeq
        val cs = File(ProgramHandlingUtil.TEMP_DIR.toString).toDirectory.walk.toSeq
        fs.filter(_.name.contains(".jar")).map(_.name).toList shouldBe List("HelloWorld.jar")
        cs.count(_.name.contains(".class")) shouldBe 0
      case Failure(x: Throwable) =>
        fail("Unable to view test repository", x)
    }
  }

  "should reflect the correct package order" in {
    val List(foo) = cpg.typeDecl.fullNameExact("Foo").l
    foo.name shouldBe "Foo"

    val List(bar) = cpg.typeDecl.fullNameExact("pac.Bar").l
    bar.name shouldBe "Bar"

    cpg.method.filterNot(_.isExternal).fullName.toSet shouldBe Set(
      "Foo.<init>:void()",
      "Foo.add:int(int,int)",
      "pac.Bar.sub:int(int,int)",
      "pac.Bar.<init>:void()"
    )
  }

}
