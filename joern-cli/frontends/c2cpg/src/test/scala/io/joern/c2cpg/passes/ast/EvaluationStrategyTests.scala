package io.joern.c2cpg.passes.ast

import io.joern.c2cpg.Config
import io.joern.c2cpg.testfixtures.C2CpgSuite
import io.shiftleft.codepropertygraph.generated.EvaluationStrategies.{BY_REFERENCE, BY_SHARING, BY_VALUE}
import io.shiftleft.semanticcpg.language.*

class EvaluationStrategyTests extends C2CpgSuite {

  "The parameters of a C function" should {
    val cpg = code("""
        |typedef int count_t;
        |typedef int *ptr_t;
        |enum Color { RED };
        |struct Point { int x; int y[2]; };
        |struct Node { struct Node *next; };
        |struct Opaque;
        |#define RESET(x) x = 0
        |void f(int i, count_t c, enum Color e, struct Point p, int *ip, ptr_t pt, int a[4], int (*cb)(int),
        |       struct Node n, unknown_t u, ...) {
        |  RESET(i);
        |}
        |void g(struct Opaque o);
        |int k(r) int r; { return r; }
        |""".stripMargin).withConfig(Config().withByValueArguments(true))

    "be by value where the function receives a copy that shares nothing with the argument" in {
      cpg.method.nameExact("f").parameter.map(p => p.name -> p.evaluationStrategy).toMap shouldBe Map(
        "i"         -> BY_VALUE,
        "c"         -> BY_VALUE,
        "e"         -> BY_VALUE,
        "p"         -> BY_VALUE,
        "ip"        -> BY_SHARING,
        "pt"        -> BY_SHARING,
        "a"         -> BY_SHARING,
        "cb"        -> BY_SHARING,
        "n"         -> BY_SHARING,
        "u"         -> BY_SHARING,
        "<param>11" -> BY_VALUE
      )
    }

    "keep their strategy in the output parameter" in {
      cpg.method.nameExact("f").parameter.nameExact("ip").asOutput.evaluationStrategy.l shouldBe List(BY_SHARING)
    }

    "be by sharing for a struct whose definition is not known" in {
      cpg.method.nameExact("g").parameter.evaluationStrategy.l shouldBe List(BY_SHARING)
    }

    "be decided for a K&R parameter" in {
      cpg.method.nameExact("k").parameter.evaluationStrategy.l shouldBe List(BY_VALUE)
    }

    "be by reference for a macro, which may assign its argument" in {
      cpg.method.nameExact("RESET").parameter.evaluationStrategy.l shouldBe List(BY_REFERENCE)
    }
  }

  "The parameters of a C++ function" should {
    val cpg = code(
      """
        |using ref_t = int&;
        |struct Value { int x; };
        |struct Derived : Value { double d; };
        |struct Holder : Value { int *p; };
        |struct Incomplete;
        |void f(int &r, const int &cr, int &&rr, ref_t tr, Value v, Derived d, Holder h, int &last, ...) {}
        |void g(Incomplete i);
        |template <typename T> void t(T byValue, T &byRef, T &&forward) {}
        |template <typename... Args> void pack(Args &&...args) {}
        |struct S { void m(int v) {} };
        |auto lambda = [](int &r, int v) {};
        |struct CopyWrites { int v; CopyWrites(CopyWrites &other) : v(other.v) { other.v = 0; } };
        |struct Converts { int v; operator int() { return v++; } };
        |void copies(CopyWrites c, Converts k) {}
        |""".stripMargin,
      "Test0.cpp"
    ).withConfig(Config().withByValueArguments(true))

    "be by reference for a reference" in {
      cpg.method.nameExact("f").parameter.map(p => p.name -> p.evaluationStrategy).toMap shouldBe Map(
        "r"        -> BY_REFERENCE,
        "cr"       -> BY_REFERENCE,
        "rr"       -> BY_REFERENCE,
        "tr"       -> BY_REFERENCE,
        "v"        -> BY_VALUE,
        "d"        -> BY_VALUE,
        "h"        -> BY_SHARING,
        "last"     -> BY_REFERENCE,
        "<param>9" -> BY_VALUE
      )
    }

    "be by sharing for a class whose definition is not known" in {
      cpg.method.nameExact("g").parameter.evaluationStrategy.l shouldBe List(BY_SHARING)
    }

    "be by sharing for a type that depends on a template parameter, unless it is a reference" in {
      cpg.method.nameExact("t").parameter.map(p => p.name -> p.evaluationStrategy).toMap shouldBe Map(
        "byValue" -> BY_SHARING,
        "byRef"   -> BY_REFERENCE,
        "forward" -> BY_REFERENCE
      )
      cpg.method.nameExact("pack").parameter.evaluationStrategy.l shouldBe List(BY_REFERENCE, BY_REFERENCE)
    }

    "keep this by sharing" in {
      cpg.method.nameExact("m").parameter.map(p => p.name -> p.evaluationStrategy).toMap shouldBe Map(
        "this" -> BY_SHARING,
        "v"    -> BY_VALUE
      )
    }

    "be by sharing for a class whose copy constructor is user-provided, which may write the argument" in {
      cpg.method.nameExact("copies").parameter.map(p => p.name -> p.evaluationStrategy).toMap shouldBe Map(
        "c" -> BY_SHARING,
        "k" -> BY_VALUE
      )
    }

    "be decided for a lambda" in {
      cpg.method.nameExact("<lambda>0").parameter.map(p => p.name -> p.evaluationStrategy).toMap shouldBe Map(
        "r" -> BY_REFERENCE,
        "v" -> BY_VALUE
      )
    }
  }

  "The parameters of a function without joern.byValueArguments" should {
    val cpg = code("""
        |struct Point { int x; };
        |#define RESET(x) x = 0
        |void f(int i, int *ip, struct Point p, int a[4], ...) { RESET(i); }
        |""".stripMargin)

    "all be by value, as the default" in {
      cpg.method.nameExact("f").parameter.evaluationStrategy.toSet shouldBe Set(BY_VALUE)
      cpg.method.nameExact("RESET").parameter.evaluationStrategy.l shouldBe List(BY_VALUE)
    }
  }
}
