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
        "r"       -> List("by_value"),
        "a"       -> List("by_value")
      )
      cpg.local.nameExact("fileStatic")._tagViaTaggedByOut.nameExact(X2CpgDefines.StorageClassTag).l shouldBe empty
    }
  }

  "A scalar variable in a C function" should {
    val cpg = code("""
        |#include <stddef.h>
        |typedef unsigned long len_t;
        |enum color { RED, GREEN };
        |struct point { int x; };
        |int use(int v);
        |int logf_(const char *fmt, ...);
        |void fill(int *p);
        |int f(int n, int *out, int taken) {
        |  len_t len = 1;
        |  enum color c = RED;
        |  int viaPointer = 2;
        |  int (*fp)(int) = use;
        |  int unknown = 3;
        |  int macroArg = 4;
        |  int arr[2];
        |  struct point s;
        |  int addressed = 5;
        |  int *p = &addressed;
        |  use(n);
        |  logf_("%d %d", len, (c));
        |  fp(viaPointer);
        |  undeclared(unknown);
        |  MISSING_MACRO(macroArg);
        |  fill(&taken);
        |  return use(s.x) + *p + arr[0] + *out;
        |}
        |""".stripMargin)

    def tags(name: String): Map[String, List[String]] = {
      val method = cpg.method.nameExact(name).l
      (method.parameter.l.map(param => param.name -> param._tagViaTaggedByOut.l) ++
        method.local.l.map(local => local.name -> local._tagViaTaggedByOut.l)).map { case (variable, tags) =>
        variable -> tags.filter(_.name == X2CpgDefines.StorageClassTag).map(_.value)
      }.toMap
    }

    "be tagged by_value when every call it is passed to receives it through a scalar parameter or `...`" in {
      tags("f") shouldBe Map(
        "n"          -> List("by_value"),
        "out"        -> List(),
        "taken"      -> List(),
        "len"        -> List("by_value"),
        "c"          -> List("by_value"),
        "viaPointer" -> List("by_value"),
        "fp"         -> List(),
        "unknown"    -> List(),
        "macroArg"   -> List(),
        "arr"        -> List(),
        "s"          -> List(),
        "addressed"  -> List(),
        "p"          -> List()
      )
    }
  }

  "A scalar variable that a GNU C nested function uses" should {
    val cpg = code("""
        |void outer(void) {
        |  int nested = 0;
        |  void inner(void) { nested++; }
        |  inner();
        |}
        |""".stripMargin)

    "not be tagged" in {
      cpg.method.nameExact("outer").local.nameExact("nested")._tagViaTaggedByOut.l shouldBe empty
    }
  }

  "A scalar variable in a C++ function" should {
    val cpg = code(
      """
        |#include <utility>
        |void byval(int v);
        |void byref(int &r);
        |void byconst(const int &r);
        |template <typename T> void copy(T t);
        |template <typename T> void refer(T &t);
        |template <typename T> void forward(T &&t);
        |struct Out { Out &operator<<(int v); Out &operator>>(int &v); };
        |struct ByVal { ByVal(int v); };
        |struct ByRef { ByRef(int &v); };
        |struct Holder { int &ref; };
        |struct Widget { void set(int v); void get(int &v); };
        |enum Color { Red };
        |Color &operator++(Color &c);
        |struct Chained { int &r; Chained(int chained) : r(chained) {} };
        |void takeHolder(Holder h);
        |int &dangling() { int returned = 0; return returned; }
        |void f(Out out, Widget w) {
        |  int plain = 0, member = 0, shifted = 0, constructed = 0, deduced = 0, copiedByLambda = 0, cast = 0;
        |  int reference = 0, constReference = 0, memberRef = 0, extracted = 0, refConstructed = 0, braced = 0;
        |  int newed = 0, temporary = 0, referred = 0, explicitRef = 0, forwarded = 0, bound = 0, aggregate = 0;
        |  int capturedByRef = 0, defaultByRef = 0, castRef = 0, conditional = 0, initCaptured = 0;
        |  int bracedByVal = 0, newedByVal = 0, temporaryByVal = 0, bracedTemporaryByVal = 0;
        |  int bracedTemporary = 0, commaOperand = 0, listArgument = 0;
        |  Color unaryOperand = Red;
        |  byval(plain);
        |  w.set(member);
        |  out << shifted;
        |  ByVal b(constructed);
        |  copy(deduced);
        |  [=]() { byval(copiedByLambda); }();
        |  byval(static_cast<long>(cast));
        |  ByVal b2{bracedByVal};
        |  ByVal *heapByVal = new ByVal(newedByVal);
        |  auto madeByVal = ByVal(temporaryByVal);
        |  auto bracedMadeByVal = ByVal{bracedTemporaryByVal};
        |  byref(reference);
        |  byconst(constReference);
        |  w.get(memberRef);
        |  out >> extracted;
        |  ByRef r(refConstructed);
        |  ByRef r2{braced};
        |  ByRef *heap = new ByRef(newed);
        |  auto made = ByRef(temporary);
        |  refer(referred);
        |  copy<int &>(explicitRef);
        |  forward(forwarded);
        |  int &alias = bound;
        |  Holder h{aggregate};
        |  [&capturedByRef]() { capturedByRef++; }();
        |  [&]() { defaultByRef++; }();
        |  static_cast<int &>(castRef) = 1;
        |  int &either = true ? conditional : bound;
        |  [&ref = initCaptured]() { ref++; }();
        |  auto bracedMade = ByRef{bracedTemporary};
        |  int &viaComma = (0, commaOperand);
        |  takeHolder({listArgument});
        |  ++unaryOperand;
        |  auto lambda = [](int param) { byval(param); };
        |}
        |""".stripMargin,
      s"Test0${FileDefaults.CppExt}"
    )

    def tagOf(name: String): List[String] =
      (cpg.local.nameExact(name)._tagViaTaggedByOut.l ++ cpg.parameter.nameExact(name)._tagViaTaggedByOut.l)
        .filter(_.name == X2CpgDefines.StorageClassTag)
        .map(_.value)

    "be tagged by_value when every call it is passed to receives it by value" in {
      val byValue = List(
        "plain",
        "member",
        "shifted",
        "constructed",
        "bracedByVal",
        "newedByVal",
        "temporaryByVal",
        "bracedTemporaryByVal",
        "deduced",
        "copiedByLambda",
        "cast"
      )
      byValue.map(name => name -> tagOf(name)) shouldBe byValue.map(_ -> List("by_value"))
      cpg.parameter.nameExact("param")._tagViaTaggedByOut.value.l shouldBe List("by_value")
    }

    "not be tagged when it may be bound to a reference" in {
      val bound = List(
        "reference",
        "constReference",
        "memberRef",
        "extracted",
        "refConstructed",
        "braced",
        "newed",
        "temporary",
        "referred",
        "explicitRef",
        "forwarded",
        "bound",
        "aggregate",
        "capturedByRef",
        "defaultByRef",
        "castRef",
        "conditional",
        "initCaptured",
        "bracedTemporary",
        "commaOperand",
        "listArgument",
        "unaryOperand",
        "chained",
        "returned"
      )
      (cpg.local.name.l ++ cpg.parameter.name.l) should contain allElementsOf bound
      bound.map(name => name -> tagOf(name)) shouldBe bound.map(_ -> List())
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
