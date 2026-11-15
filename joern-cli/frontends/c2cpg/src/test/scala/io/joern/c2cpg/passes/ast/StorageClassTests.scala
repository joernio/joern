package io.joern.c2cpg.passes.ast

import io.joern.c2cpg.parser.FileDefaults
import io.joern.c2cpg.testfixtures.C2CpgSuite
import io.joern.x2cpg.Defines as X2CpgDefines
import io.shiftleft.semanticcpg.language.*

class StorageClassTests extends C2CpgSuite {

  "A variable declared in a C function" should {
    val cpg = code("""
        |#define STATIC static
        |int g;
        |static int fileStatic;
        |struct Point { int x; };
        |void f(void) {
        |  static int s;
        |  static int counter = 0, limit = 8;
        |  static char buf[4];
        |  static struct Point origin = {0};
        |  STATIC int m;
        |  extern int g;
        |  register int r;
        |  int a;
        |}
        |""".stripMargin)

    "be tagged with a storage class that outlives a call of the function, and only then" in {
      val tagged = cpg.method
        .nameExact("f")
        .local
        .l
        .map { local =>
          local.name -> local._tagViaTaggedByOut.nameExact(X2CpgDefines.StorageClassTag).value.l
        }
        .toMap
      tagged shouldBe Map(
        "s"       -> List("static"),
        "counter" -> List("static"),
        "limit"   -> List("static"),
        "buf"     -> List("static"),
        "origin"  -> List("static"),
        "m"       -> List("static"),
        "g"       -> List("extern"),
        "r"       -> List(),
        "a"       -> List()
      )
      cpg.local.nameExact("fileStatic")._tagViaTaggedByOut.nameExact(X2CpgDefines.StorageClassTag).l shouldBe empty
    }
  }

  "A thread-local variable in a C++ function" should {
    val cpg = code(
      """
        |void f() {
        |  thread_local int t = 0;
        |  static thread_local int u = 0;
        |}
        |""".stripMargin,
      s"Test0${FileDefaults.CppExt}"
    )

    "be tagged as such, or as static when it is both" in {
      cpg.method.nameExact("f").local.nameExact("t")._tagViaTaggedByOut.value.l shouldBe List("thread_local")
      cpg.method.nameExact("f").local.nameExact("u")._tagViaTaggedByOut.value.l shouldBe List("static")
    }
  }

}
