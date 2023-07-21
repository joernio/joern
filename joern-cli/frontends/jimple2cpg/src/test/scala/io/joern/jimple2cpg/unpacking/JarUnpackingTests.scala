package io.joern.jimple2cpg.unpacking

import better.files.File
import io.joern.jimple2cpg.{Config, Jimple2Cpg}
import io.joern.jimple2cpg.util.ProgramHandlingUtil
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.utils.ProjectRoot
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.must.Matchers
import org.scalatest.matchers.should.Matchers.*
import org.scalatest.wordspec.AnyWordSpec

import java.nio.file.{Files, Path, Paths}
import scala.util.{Failure, Success, Try}

class JarUnpackingTests extends AnyWordSpec with Matchers with BeforeAndAfterAll {

  var validCpgs: Map[String, Cpg] = _
  var slippyCpg: Cpg              = _

  override protected def beforeAll(): Unit = {
    super.beforeAll()
    validCpgs = List("HelloWorld.jar", "NestedHelloWorld.jar", "helloworld")
      .map(k => (k, getUnpackingCpg(k)))
      .toMap
    slippyCpg = getUnpackingCpg("slippy.zip")
  }

  private def getUnpackingCpg(path: String): Cpg =
    Try(getClass.getResource(s"/unpacking/${path}").toURI) match {
      case Success(x) =>
        implicit val config: Config = Config().withRecurse(true)
        new Jimple2Cpg().createCpg(Paths.get(x).toString).get
      case Failure(x: Throwable) =>
        fail("Unable to obtain test resources.", x)
    }

  "'resources/unpacking' should contain 'HelloWorld.jar' and 'NestedHelloWorld.jar'" in {
    val targetDir = ProjectRoot.relativise("joern-cli/frontends/jimple2cpg/src/test/resources/unpacking")
    File(targetDir)
      .walk()
      .filter(f => f.isRegularFile && f.extension.exists(_ == ".jar"))
      .map(_.name)
      .toSet shouldBe Set("HelloWorld.jar", "NestedHelloWorld.jar")
  }

  "should reflect the correct package order" in {
    for ((name, cpg) <- validCpgs) {
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

  "should not extract zip entries with paths that contain '..' ala zipslip" in {
    slippyCpg.typeDecl shouldBe empty
    slippyCpg.method.filterNot(_.isExternal) shouldBe empty
  }

}
