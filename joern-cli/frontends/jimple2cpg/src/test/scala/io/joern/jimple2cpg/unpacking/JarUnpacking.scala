package io.joern.jimple2cpg.unpacking

import io.joern.jimple2cpg.{Config, Jimple2Cpg}
import io.joern.jimple2cpg.util.ProgramHandlingUtil
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.semanticcpg.language._
import io.shiftleft.utils.ProjectRoot
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.must.Matchers
import org.scalatest.matchers.should.Matchers._
import org.scalatest.wordspec.AnyWordSpec

import java.nio.file.{Files, Path, Paths}
import scala.util.{Failure, Success, Try}

class JarUnpacking extends AnyWordSpec with Matchers with BeforeAndAfterAll {

  var cpg: Cpg = _

  override protected def beforeAll(): Unit = {
    super.beforeAll()
    Try(getClass.getResource("/unpacking").toURI) match {
      case Success(x) =>
        implicit val defaultConfig: Config = Config()
        cpg = new Jimple2Cpg().createCpg(Paths.get(x).toString).get
      case Failure(x: Throwable) =>
        fail("Unable to obtain test resources.", x)
    }
  }

  "should extract files and clean up temp directory" in {
    val targetDir = ProjectRoot.relativise("joern-cli/frontends/jimple2cpg/src/test/resources/unpacking")
    Files
      .walk(Path.of(targetDir))
      .map(_.toFile)
      .filter(f => f.isFile && f.getName.contains(".jar"))
      .map(_.getName)
      .toArray shouldBe Array("HelloWorld.jar")
    ProgramHandlingUtil.getUnpackingDir.toFile.length() shouldBe 0
  }

  "should reflect the correct package order" in {
    val List(foo) = cpg.typeDecl.fullNameExact("Foo").l
    foo.name shouldBe "Foo"

    val List(bar) = cpg.typeDecl.fullNameExact("pac.Bar").l
    bar.name shouldBe "Bar"

    cpg.method.filterNot(_.isExternal).fullName.toSetMutable shouldBe Set(
      "Foo.<init>:void()",
      "Foo.add:int(int,int)",
      "pac.Bar.sub:int(int,int)",
      "pac.Bar.<init>:void()"
    )
  }

}
