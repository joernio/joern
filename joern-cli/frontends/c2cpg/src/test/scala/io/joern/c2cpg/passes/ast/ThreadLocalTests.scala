package io.joern.c2cpg.passes.ast

import io.joern.c2cpg.testfixtures.C2CpgSuite
import io.shiftleft.codepropertygraph.generated.nodes.Unknown
import io.shiftleft.semanticcpg.language.*

class ThreadLocalTests extends C2CpgSuite {

  "C11 thread-local variables" should {
    val cpg = code("""
        |_Thread_local int a;
        |static _Thread_local int b;
        |extern _Thread_local unsigned long c;
        |void f(void) {
        |  static _Thread_local int d;
        |  extern _Thread_local int e;
        |  static __thread int g;
        |  d = e + g;
        |}
        |""".stripMargin)

    "be declared at file scope with their names and types" in {
      cpg.method
        .nameExact("<global>")
        .local
        .map(local => local.name -> local.typeFullName)
        .l
        .sortBy(_._1) shouldBe List("a" -> "int", "b" -> "int", "c" -> "unsigned long")
    }

    "be declared at block scope with their names and types, and referenced by their uses" in {
      cpg.method.nameExact("f").local.map(local => local.name -> local.typeFullName).l.sortBy(_._1) shouldBe List(
        "d" -> "int",
        "e" -> "int",
        "g" -> "int"
      )
      cpg.method
        .nameExact("f")
        .ast
        .isIdentifier
        .map(identifier => identifier.name -> identifier.refsTo.name.l)
        .l
        .sortBy(_._1) shouldBe List("d" -> List("d"), "e" -> List("e"), "g" -> List("g"))
      cpg.all.collectAll[Unknown].code.l shouldBe empty
    }
  }
}
