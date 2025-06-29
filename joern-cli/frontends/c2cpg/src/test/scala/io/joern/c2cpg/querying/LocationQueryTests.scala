package io.joern.c2cpg.querying

import io.joern.c2cpg.testfixtures.C2CpgSuite
import io.shiftleft.semanticcpg.language.*

class LocationQueryTests extends C2CpgSuite {

  private implicit val finder: NodeExtensionFinder = DefaultNodeExtensionFinder

  private val cpg = code("""
   |int my_func(int param1) {
   |   int x = foo(param1);
   |}""".stripMargin)

  "should return location for method" in {
    val locations = cpg.method.name("my_func").location.l
    locations.size shouldBe 1

    val loc = locations.head
    loc.methodFullName shouldBe "my_func"
    loc.methodShortName shouldBe "my_func"
    loc.symbol shouldBe "my_func"
    loc.lineNumber shouldBe Option(2)
    loc.filename should endWith(".c")
    loc.nodeLabel shouldBe "METHOD"

  }

  "should return location for parameter" in {
    val locations = cpg.parameter.name("param1").location.l
    locations.size shouldBe 1

    val loc = locations.head
    loc.methodFullName shouldBe "my_func"
    loc.methodShortName shouldBe "my_func"
    loc.symbol shouldBe "param1"
    loc.lineNumber shouldBe Option(2)
    loc.filename should endWith(".c")
    loc.nodeLabel shouldBe "METHOD_PARAMETER_IN"

  }

  "should return location for return parameter" in {
    val locations = cpg.method.name("my_func").methodReturn.location.l
    locations.size shouldBe 1

    val loc = locations.head
    loc.methodFullName shouldBe "my_func"
    loc.methodShortName shouldBe "my_func"
    loc.symbol shouldBe "$ret"
    loc.lineNumber shouldBe Option(2)
    loc.filename should endWith(".c")
    loc.nodeLabel shouldBe "METHOD_RETURN"

  }

  "should return location for call" in {
    val locations = cpg.call.name("foo").location.l
    locations.size shouldBe 1

    val loc = locations.head
    loc.methodFullName shouldBe "my_func"
    loc.methodShortName shouldBe "my_func"
    loc.symbol shouldBe "foo(param1)"
    loc.lineNumber shouldBe Option(3)
    loc.filename should endWith(".c")
    loc.nodeLabel shouldBe "CALL"

  }

  "should return location for identifier" in {
    val locations = cpg.identifier.name("x").location.l
    locations.size shouldBe 1

    val loc = locations.head
    loc.methodFullName shouldBe "my_func"
    loc.methodShortName shouldBe "my_func"
    loc.symbol shouldBe "x"
    loc.lineNumber shouldBe Option(3)
    loc.filename should endWith(".c")
    loc.nodeLabel shouldBe "IDENTIFIER"
  }

  "should return location for local" in {
    val locations = cpg.local.name("x").location.l
    locations.size shouldBe 1

    val loc = locations.head
    loc.methodFullName shouldBe "my_func"
    loc.methodShortName shouldBe "my_func"
    loc.symbol shouldBe "x"
    loc.lineNumber shouldBe Option(3)
    loc.filename should endWith(".c")
    loc.nodeLabel shouldBe "LOCAL"
  }

  "querying member location" should {
    val cpg = code(
      """
        |#include <stdlib.h>
        |
        |struct flexArrayStruct {
        |  int num;
        |};
        |""".stripMargin,
      "foo.c"
    )

    "have the correct location for a struct member" in {
      inside(cpg.member.name("num").location.l) { case List(location) =>
        location.className shouldBe "flexArrayStruct"
        location.filename shouldBe "foo.c"
        location.lineNumber shouldBe Some(5)
        location.symbol shouldBe "num"
        location.classShortName shouldBe "flexArrayStruct"
        location.methodFullName shouldBe "foo.c:<global>"
        location.methodShortName shouldBe "<global>"
      }
    }
  }

}
