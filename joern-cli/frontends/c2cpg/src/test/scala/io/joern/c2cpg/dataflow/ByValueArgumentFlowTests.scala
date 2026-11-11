package io.joern.c2cpg.dataflow

import io.joern.c2cpg.parser.FileDefaults
import io.joern.c2cpg.testfixtures.DataFlowCodeToCpgSuite
import io.shiftleft.codepropertygraph.generated.{Cpg, EdgeTypes}
import io.shiftleft.codepropertygraph.generated.nodes.{CfgNode, Identifier}
import io.shiftleft.semanticcpg.language.*

/** C passes arguments by value, so a function cannot write a scalar variable of its caller that is passed to it by
  * name: the call is no definition of that variable.
  */
class ByValueArgumentFlowTests extends DataFlowCodeToCpgSuite {

  /** The variables that the argument `name` of the calls of `callee` in `method` is stored as defining: the nodes its
    * reaching-definition edges lead to, other than the call it is an argument of.
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
        argument.outE(EdgeTypes.REACHING_DEF).map(_.dst).collect {
          case node: CfgNode if !argument.inCall.contains(node) => node.code
        }
      }

  /** The code of the identifiers whose definitions reach the argument of the call of `sink` in `method`. */
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

  "A scalar passed by value in a C file" should {
    val cpg = code("""
        |typedef unsigned long size_t;
        |enum color { RED, GREEN };
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
        |""".stripMargin)

    "keep the definitions the call makes" in {
      definedBy(cpg, "pointer", "extp", "p") should not be empty
      definedBy(cpg, "taken", "ext", "t") should not be empty
      definedBy(cpg, "global", "ext", "g") should not be empty
      definedBy(cpg, "bystruct", "exts", "s") should not be empty
    }

    "keep the definitions of macro arguments and assignment targets" in {
      definitionsReachingSink(cpg, "macro") should contain("n")
      definitionsReachingSink(cpg, "assigned") should contain("n")
    }
  }

  "A scalar passed in a C++ file" should {
    val cpg = code(
      """
        |void inc(int& x) { x++; }
        |void sink(int v);
        |void byref(int n) { inc(n); sink(n); }
        |""".stripMargin,
      s"Test0${FileDefaults.CppExt}"
    )

    "keep the definition, since the parameter may be a reference" in {
      definedBy(cpg, "byref", "inc", "n") should not be empty
    }
  }

}
