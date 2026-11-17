package io.joern.c2cpg.dataflow

import io.joern.c2cpg.Config
import io.joern.c2cpg.parser.FileDefaults
import io.joern.c2cpg.testfixtures.{DataFlowCodeToCpgSuite, DataFlowTestCpg}
import io.joern.dataflowengineoss.layers.dataflows.{OssDataFlow, OssDataFlowOptions}
import io.joern.x2cpg.X2Cpg
import io.joern.x2cpg.testfixtures.Code2CpgFixture
import io.shiftleft.codepropertygraph.generated.{Cpg, EdgeTypes}
import io.shiftleft.codepropertygraph.generated.nodes.{CfgNode, Identifier}
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.layers.LayerCreatorContext

/** A graph c2cpg builds and whose reaching definitions it solves as with the system property `joern.byValueArguments`.
  */
class ByValueDataFlowTestCpg extends DataFlowTestCpg {
  override def applyPasses(): Unit = {
    X2Cpg.applyDefaultOverlays(this)
    new OssDataFlow(new OssDataFlowOptions(byValueArguments = true)).run(new LayerCreatorContext(this))
  }
}

/** The variables that the argument `name` of the calls of `callee` in `method` is stored as defining: the nodes of
  * `method` outside the call that its reaching-definition edges lead to, by their code, sorted.
  */
private def definedBy(cpg: Cpg, method: String, callee: String, name: String): List[String] =
  cpg.method
    .nameExact(method)
    .call
    .nameExact(callee)
    .argument
    .isIdentifier
    .nameExact(name)
    .l
    .flatMap { argument =>
      val call = argument.inCall.head
      argument.outE(EdgeTypes.REACHING_DEF).map(_.dst).collect {
        case node: CfgNode if node.method == argument.method && node != call && !call.argument.contains(node) =>
          node.code
      }
    }
    .sorted

/** With `joern.byValueArguments`, a function cannot write a variable of its caller that it receives through a parameter
  * whose evaluation strategy is `BY_VALUE`, so the call is no definition of that variable.
  */
class ByValueArgumentFlowTests
    extends Code2CpgFixture(() => new ByValueDataFlowTestCpg().withConfig(Config().withByValueArguments(true))) {

  /** The code of the identifiers whose definitions reach the argument of the call of `sink` in `method`, sorted. */
  private def definitionsReachingSink(cpg: Cpg, method: String): List[String] =
    cpg.method
      .nameExact(method)
      .call
      .nameExact("sink")
      .argument
      .inE(EdgeTypes.REACHING_DEF)
      .map(_.src)
      .collect { case identifier: Identifier => identifier.code }
      .l
      .sorted

  "A scalar passed by value in a C file" should {
    val cpg = code("""
        |typedef unsigned long size_t;
        |enum color { RED, GREEN };
        |void touch(int *p);
        |int cond(void);
        |void use(int v) { }
        |void ext(int v);
        |void sink(int v);
        |
        |void guarded(int n) {
        |  if (cond()) use(n);
        |  if (cond()) use(n);
        |  sink(n);
        |}
        |void kinds(size_t len, enum color c) {
        |  int local = 1;
        |  ext(len); ext(c); ext(local);
        |  sink(len + c + local);
        |}
        |""".stripMargin)

    "not be a definition made by a call of a defined function" in {
      definedBy(cpg, "guarded", "use", "n") shouldBe empty
    }

    "not be a definition made by a call of a declared function, whether a local, a typedef or an enum" in {
      definedBy(cpg, "kinds", "ext", "local") shouldBe empty
      definedBy(cpg, "kinds", "ext", "len") shouldBe empty
      definedBy(cpg, "kinds", "ext", "c") shouldBe empty
    }
  }

  "A variable a call may still write" should {
    val cpg = code("""
        |struct pair { int a; int *p; };
        |int g;
        |void ext(int v);
        |void extp(int *p);
        |void exts(struct pair s);
        |void sink(int v);
        |#define SET(x) ((x) = 1)
        |
        |void pointer(int *p) { extp(p); sink(*p); }
        |void taken(int n) { int t = n; int *q = &t; ext(t); sink(t); }
        |void global(void) { ext(g); sink(g); }
        |void bystruct(struct pair s) { exts(s); sink(s.a); }
        |void macro(int n) { SET(n); sink(n); }
        |void assigned(int n) { n = 2; sink(n); }
        |void unparsedMacro(int sr) { ENTER_CRITICAL(sr); sink(sr); }
        |void (*fp)(int);
        |void viaPointer(int n) { fp(n); sink(n); }
        |""".stripMargin)

    "keep the definitions the call makes" in {
      definedBy(cpg, "pointer", "extp", "p") shouldBe List("RET", "int *p", "p")
      definedBy(cpg, "taken", "ext", "t") shouldBe List("t")
      definedBy(cpg, "global", "ext", "g") shouldBe List("<global> g")
      definedBy(cpg, "bystruct", "exts", "s") shouldBe List("RET", "s", "s.a", "struct pair s")
    }

    "keep the definitions of macro arguments and assignment targets" in {
      definitionsReachingSink(cpg, "macro") shouldBe List("n", "n")
      definitionsReachingSink(cpg, "assigned") shouldBe List("n")
    }

    "keep the definition made by a call without parameter types, such as a macro whose definition is missing" in {
      definedBy(cpg, "unparsedMacro", "ENTER_CRITICAL", "sr") shouldBe List("RET", "int sr", "sr")
    }

    "keep the definition made by a call through a function pointer" in {
      definitionsReachingSink(cpg, "viaPointer") shouldBe List("n")
    }
  }

  "A scalar passed in a C++ file" should {
    val cpg = code(
      """
        |void byval(int v);
        |void byref(int& v);
        |void var(int a, ...);
        |template <typename T> void tmpl(T v);
        |template <typename T> void forward(T&& v);
        |template <typename T> void tref(T& v);
        |struct S { void m(int v); };
        |void sink(int v);
        |
        |void plain(int n) { byval(n); sink(n); }
        |void variadic(int n) { var(1, n); sink(n); }
        |void member(S s, int n) { s.m(n); sink(n); }
        |void reference(int n) { byref(n); sink(n); }
        |void templated(int n) { tmpl(n); sink(n); }
        |void explicitReference(int n) { tmpl<int&>(n); sink(n); }
        |void forwarding(int n) { forward(n); sink(n); }
        |void templateReference(int n) { tref(n); sink(n); }
        |void unresolved(int n) { undeclared(n); sink(n); }
        |void alsoByReference(int n) { byref(n); byval(n); sink(n); }
        |void boundReference(int n) { int& r = n; byval(n); sink(n); }
        |void captured(int n) { auto l = [&]() { n++; }; byval(n); l(); sink(n); }
        |""".stripMargin,
      s"Test0${FileDefaults.CppExt}"
    )

    "not be a definition by a call whose parameter is a scalar" in {
      definedBy(cpg, "plain", "byval", "n") shouldBe empty
      definedBy(cpg, "member", "m", "n") shouldBe empty
    }

    "keep the definition by a call that takes it through `...` or through a parameter whose type is a template parameter" in {
      definedBy(cpg, "variadic", "var", "n") shouldBe List("RET", "int n", "n")
      definedBy(cpg, "templated", "tmpl", "n") shouldBe List("n")
    }

    "keep the definition when the parameter may be a reference" in {
      definedBy(cpg, "reference", "byref", "n") shouldBe List("n")
      definedBy(cpg, "explicitReference", "tmpl<int&>", "n") shouldBe List("n")
      definedBy(cpg, "forwarding", "forward", "n") shouldBe List("n")
      definedBy(cpg, "templateReference", "tref", "n") shouldBe List("n")
      definedBy(cpg, "unresolved", "undeclared", "n") shouldBe List("n")
    }

    "keep the definition by a call that takes it by value when a reference to it may have escaped" in {
      definedBy(cpg, "alsoByReference", "byval", "n") shouldBe List("n")
      definedBy(cpg, "alsoByReference", "byref", "n") shouldBe List("n")
      definedBy(cpg, "boundReference", "byval", "n") shouldBe List("n")
      definedBy(cpg, "captured", "byval", "n") shouldBe List("n")
      definedBy(cpg, "plain", "byval", "n") shouldBe empty
    }
  }

  "A scalar passed in a header file" should {
    val cpg = code(
      """
        |void use(int v);
        |void sink(int v);
        |static inline void inC(int n) { use(n); sink(n); }
        |""".stripMargin,
      "c.h"
    ).moreCode(
      """
        |void byref(int& v);
        |void sinkCpp(int v);
        |inline void inCpp(int n) { byref(n); sinkCpp(n); }
        |""".stripMargin,
      "cpp.h"
    ).moreCode(
      """
        |void useBoth(int v);
        |void sinkBoth(int v);
        |static inline void inBoth(int n) { useBoth(n); sinkBoth(n); }
        |""".stripMargin,
      "both.h"
    ).moreCode(
      """
        |#include "c.h"
        |#include "both.h"
        |int mainC(void) { return 0; }
        |""".stripMargin,
      "main.c"
    ).moreCode(
      """
        |#include "cpp.h"
        |#include "both.h"
        |int mainCpp() { return 0; }
        |""".stripMargin,
      "main.cpp"
    )

    "not be a definition by a call that takes it by value, whether the header is parsed as C, as C++ or as both" in {
      definedBy(cpg, "inC", "use", "n") shouldBe empty
      cpg.method.nameExact("inBoth").size shouldBe 2
      definedBy(cpg, "inBoth", "useBoth", "n") shouldBe empty
    }

    "keep the definition by a call that takes it by reference" in {
      definedBy(cpg, "inCpp", "byref", "n") shouldBe List("n")
    }
  }

  "A variable whose type name two files declare differently" should {
    val cpg = code(
      """
        |typedef int Value;
        |typedef int Count;
        |""".stripMargin,
      "a.c"
    ).moreCode(
      """
        |typedef int* Value;
        |typedef long Count;
        |int sscanf(const char* s, const char* format, ...);
        |void sink(int v);
        |void pointer(Value p) { sscanf("1", "%d", p); sink(*p); }
        |void scalar(Count n) { sscanf("1", "%ld", n); sink(n); }
        |""".stripMargin,
      "b.c"
    )

    "keep the definition when one of the declarations is not a scalar" in {
      cpg.method.nameExact("pointer").parameter.typeFullName.l shouldBe List("Value")
      definedBy(cpg, "pointer", "sscanf", "p") shouldBe List("RET", "Value p", "p")
    }

    "keep the definition when the argument is absorbed by `...`, whatever its type" in {
      definedBy(cpg, "scalar", "sscanf", "n") shouldBe List("Count n", "RET", "n")
    }
  }

  "A scalar passed to a constructor" should {
    val cpg = code(
      """
        |struct Counter { Counter(int& v) { v++; } };
        |struct Box { Box(int v) { } };
        |void sink(int v);
        |
        |void declared(int n) { Counter c(n); sink(n); }
        |void braced(int n) { Counter c{n}; sink(n); }
        |void temporary(int n) { Counter{n}; sink(n); }
        |void allocated(int n) { Counter* c = new Counter(n); sink(n); }
        |void byValue(int n) { Box b(n); sink(n); }
        |void byValueBraced(int n) { Box b{n}; sink(n); }
        |""".stripMargin,
      s"Test0${FileDefaults.CppExt}"
    )

    "keep the definition when the constructor takes it by reference, however it is called" in {
      List("declared", "braced", "temporary", "allocated").foreach { method =>
        definedBy(cpg, method, "Counter", "n") shouldBe List("n")
      }
    }

    "not be a definition when the constructor takes it by value" in {
      definedBy(cpg, "byValue", "Box", "n") shouldBe empty
      definedBy(cpg, "byValueBraced", "Box", "n") shouldBe empty
    }
  }

  "A local whose storage outlives the call" should {
    val cpg = code("""
        |#define STATIC static
        |void touch(int *p);
        |int g;
        |int depth(int v);
        |void byval(int v);
        |void sink(int v);
        |
        |int counter(int k) { static int n; n = k; depth(n); sink(n); return n; }
        |int viaMacro(int k) { STATIC int m; m = k; depth(m); sink(m); return m; }
        |void blockExtern(void) { extern int g; byval(g); sink(g); }
        |void automatic(int k) { int a = k; byval(a); sink(a); }
        |""".stripMargin)

    "keep the definition a call makes, since the callee or a call it makes can write it" in {
      definedBy(cpg, "counter", "depth", "n") shouldBe List("n")
      definedBy(cpg, "viaMacro", "depth", "m") shouldBe List("m")
      definedBy(cpg, "blockExtern", "byval", "g") shouldBe List("g")
      definedBy(cpg, "automatic", "byval", "a") shouldBe empty
    }
  }

  "An argument from which C++ initializes the parameter by running code of the program" should {
    val cpg = code(
      """
        |struct Plain { int v; };
        |struct CopyWrites { int v; CopyWrites(CopyWrites &other) : v(other.v) { other.v = 0; } };
        |struct Converts { int v; operator int() { return v++; } };
        |struct OutOfLine { int v; operator int(); };
        |OutOfLine::operator int() { return v++; }
        |struct DeclaredOnly { int v; operator long(); };
        |enum Color { RED };
        |void takesPlain(Plain p);
        |void takesCopyWrites(CopyWrites c);
        |void takesInt(int n);
        |
        |void f(Color color) {
        |  Plain p; takesPlain(p);
        |  CopyWrites c; takesCopyWrites(c);
        |  Converts k; takesInt(k);
        |  OutOfLine o; takesInt(o);
        |  DeclaredOnly d; takesInt(d);
        |  takesInt(color);
        |}
        |""".stripMargin,
      s"Test0${FileDefaults.CppExt}"
    )

    "keep the definition when copying calls a user-provided copy constructor" in {
      definedBy(cpg, "f", "takesCopyWrites", "c") shouldBe List("RET")
    }

    "keep the definition when converting may call a conversion function of the argument's class" in {
      definedBy(cpg, "f", "takesInt", "k") shouldBe List("RET")
      definedBy(cpg, "f", "takesInt", "o") shouldBe List("RET")
      definedBy(cpg, "f", "takesInt", "d") shouldBe List("RET")
    }

    "not be a definition when the copy is trivial or the conversion runs no code" in {
      definedBy(cpg, "f", "takesPlain", "p") shouldBe empty
      definedBy(cpg, "f", "takesInt", "color") shouldBe empty
    }
  }

  "An argument whose type does not resolve" should {
    val cpg = code(
      """
        |typedef missing_t size_like;
        |void takesUnsigned(unsigned int v);
        |void sink(int v);
        |void inC(size_like len) { takesUnsigned(len); takesUnsigned(UNDECLARED); sink(len + UNDECLARED); }
        |""".stripMargin,
      s"Test0${FileDefaults.CExt}"
    ).moreCode(
      """
        |typedef missing_t size_like;
        |void takesUnsignedCpp(unsigned int v);
        |void sinkCpp(int v);
        |void inCpp(size_like len) { takesUnsignedCpp(len); sinkCpp(len); }
        |""".stripMargin,
      s"Test1${FileDefaults.CppExt}"
    )

    "not be a definition in a C file, where no conversion function exists" in {
      definedBy(cpg, "inC", "takesUnsigned", "len") shouldBe empty
    }

    "keep the definition in a C++ file, where a conversion function may write it" in {
      definedBy(cpg, "inCpp", "takesUnsignedCpp", "len") shouldBe List("len")
    }

    "keep the definition of a name that is not declared, which may be a global declared in a header" in {
      definedBy(cpg, "inC", "takesUnsigned", "UNDECLARED") shouldBe List("<unknown> UNDECLARED")
    }
  }

  "A static member of a class named without its class" should {
    val cpg = code(
      """
        |void bump(int v);
        |int use(int v);
        |void touch(int *p);
        |struct Counter {
        |  static int count;
        |  int member;
        |  void step(int n) { int local = n; bump(count); use(count); bump(local); use(local); }
        |};
        |int Counter::count = 0;
        |void bump(int v) { Counter::count++; }
        |""".stripMargin,
      s"Test0${FileDefaults.CppExt}"
    )

    "keep the definition, since the callee may write the member" in {
      definedBy(cpg, "step", "bump", "count") shouldBe List("count")
    }

    "not make a declared local of the member function a definition" in {
      definedBy(cpg, "step", "bump", "local") shouldBe empty
    }
  }

  "An argument whose address a callee may keep" should {
    val cpg = code(
      """
        |int *saved;
        |void keep(int &r) { saved = &r; }
        |void bump(int v) { *saved = v + 1; }
        |int use(int v);
        |void log(const char *format, ...);
        |void g() { int x = 1; keep(x); bump(x); use(x); }
        |void logged() { int y = 1; log("%d", y); bump(y); use(y); }
        |""".stripMargin,
      s"Test0${FileDefaults.CppExt}"
    )

    "keep the definition by a later call that takes it by value" in {
      definedBy(cpg, "g", "bump", "x") shouldBe List("x")
    }

    "not keep it after the argument went through a `...`, which copies it" in {
      definedBy(cpg, "logged", "bump", "y") shouldBe empty
    }
  }

  "An argument converted by a constructor of the parameter's class" should {
    val cpg = code(
      """
        |struct Wrapper { Wrapper(int &n) { n = 0; } int x; };
        |void takesWrapper(Wrapper w);
        |int use(int v);
        |void g(int k) { int n = k; takesWrapper(n); use(n); }
        |""".stripMargin,
      s"Test0${FileDefaults.CppExt}"
    )

    "keep the definition, since the constructor may write it" in {
      definedBy(cpg, "g", "takesWrapper", "n") shouldBe List("RET", "n")
    }
  }

  "An argument whose type name is declared differently in two files" should {
    val cpg = code(
      """
        |struct Handle { int fd; };
        |void useC(struct Handle h) { }
        |""".stripMargin,
      s"Test0${FileDefaults.CExt}"
    ).moreCode(
      """
        |struct Handle { int fd; operator int() { return fd++; } };
        |void takesInt(int n);
        |int use(int v);
        |void g() { Handle h; takesInt(h); use(h.fd); }
        |""".stripMargin,
      s"Test1${FileDefaults.CppExt}"
    )

    "keep the definition when any of the declarations has a conversion function" in {
      definedBy(cpg, "g", "takesInt", "h") shouldBe List("RET", "h", "h.fd")
    }
  }

}

/** A graph c2cpg builds without `joern.byValueArguments`, every parameter `BY_VALUE`, whose reaching definitions are
  * then solved with it.
  */
class UnlabelledByValueDataFlowTestCpg extends DataFlowTestCpg {
  override def applyPasses(): Unit = {
    X2Cpg.applyDefaultOverlays(this)
    new OssDataFlow(new OssDataFlowOptions(byValueArguments = true)).run(new LayerCreatorContext(this))
  }
}

/** The data-flow switch does not trust the strategies of a graph c2cpg built without it. */
class ByValueArgumentUnlabelledFlowTests extends Code2CpgFixture(() => new UnlabelledByValueDataFlowTestCpg()) {

  "A pointer passed to a function in a graph built without the switch" should {
    val cpg = code("""
        |void touch(int *p);
        |void use(int v) { }
        |void sinkPointer(int *p);
        |void sink(int v);
        |void f(int *p, int n) { touch(p); sinkPointer(p); use(n); sink(n); }
        |""".stripMargin)

    "keep the definitions every call makes, as on master" in {
      definedBy(cpg, "f", "touch", "p") shouldBe List("p")
      definedBy(cpg, "f", "use", "n") shouldBe List("n")
    }
  }

}

/** Without `joern.byValueArguments`, as by default, a call defines every identifier argument, as it always has. */
class ByValueArgumentDefaultFlowTests extends DataFlowCodeToCpgSuite {

  "A scalar passed by value" should {
    val cpg = code("""
        |void use(int v) { }
        |void f(int n) { use(n); }
        |""".stripMargin)

    "be a definition made by the call" in {
      definedBy(cpg, "f", "use", "n") shouldBe List("RET", "int n")
    }
  }

}
