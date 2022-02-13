package io.joern.fuzzyc2cpg.querying

import io.shiftleft.codepropertygraph.generated.NodeTypes
import io.joern.fuzzyc2cpg.testfixtures.FuzzyCCodeToCpgSuite
import io.shiftleft.semanticcpg.language._

/** The following tests show in detail how queries can be started. For all node types, for which it seems reasonable,
  * all nodes of that type can be used as a starting point, e.g., `cpg.method` starts at all methods while `cpg.local`
  * starts at all locals.
  */
class NodeTypeStartersTests extends FuzzyCCodeToCpgSuite {

  override val code = """
       /* A C comment */
       // A C++ comment
       int main(int argc, char **argv) { int mylocal; libfunc(1); }
       struct foo { int x; };
    """

  "should allow retrieving files" in {
    atLeast(1, cpg.file.name.l) should endWith(".c")
  }

  /** All methods - whether defined (e.g., "main") or only referenced, e.g., "libfunc" are represented by METHOD nodes
    * in the CPG. Any method for which a definition exists is in `cpg.method.internal`. All other methods are in
    * `cpg.method.external`.
    */
  "should allow retrieving methods" in {
    cpg.method.internal.name.l shouldBe List("main")
    cpg.method.external.name.l shouldBe List("libfunc")
  }

  "should allow retrieving comments" in {
    cpg.comment.code.toSet shouldBe Set(s"/* A C comment */", s"// A C++ comment${System.lineSeparator()}")
  }

  "should allow retrieving parameters" in {
    cpg.parameter.where(_.method.internal).name.toSet shouldBe Set("argc", "argv")
  }

  "should allow retrieving locals" in {
    cpg.local.name.l shouldBe List("mylocal")
  }

  "should allow retrieving of literals" in {
    cpg.literal.code.l shouldBe List("1")
  }

  "should allow retrieving calls" in {
    cpg.call.name.l shouldBe List("libfunc")
  }

  "should allow retrieving arguments" in {
    cpg.argument.isLiteral.code.l shouldBe List("1")
  }

  "should allow retrieving type declarations" in {
    cpg.typeDecl.internal.name.toSet shouldBe Set("foo")
  }

  "should allow retrieving members" in {
    cpg.member.name.l shouldBe List("x")
  }

  "should allow retrieving (used) types" in {
    cpg.typ.name.toSet shouldBe Set("int", "void", "char * *", "ANY")
  }

  "should allow retrieving namespaces" in {
    cpg.namespace.name.l shouldBe List("<global>")
  }

  "should allow retrieving namespace blocks" in {
    cpg.namespaceBlock.name.toSet shouldBe Set("<global>")
  }

  "should allow retrieving of method returns" in {
    cpg.methodReturn.l.size shouldBe 2
  }

  "should allow retrieving of meta data" in {
    cpg.metaData.language.l shouldBe List("C")
  }

  "should allow retrieving all nodes" in {
    val allNodesLabels = cpg.all.label.toSet

    allNodesLabels shouldBe Set(
      NodeTypes.NAMESPACE_BLOCK,
      NodeTypes.MEMBER,
      NodeTypes.TYPE_DECL,
      NodeTypes.METHOD_PARAMETER_IN,
      NodeTypes.METHOD_PARAMETER_OUT,
      NodeTypes.NAMESPACE,
      NodeTypes.META_DATA,
      NodeTypes.METHOD,
      NodeTypes.FILE,
      NodeTypes.METHOD_RETURN,
      NodeTypes.TYPE,
      NodeTypes.BLOCK,
      NodeTypes.COMMENT,
      NodeTypes.LOCAL,
      NodeTypes.CALL,
      NodeTypes.LITERAL
    )
  }

  "should allow retrieving nodes by id" in {
    val method1 = cpg.method.name("main").head
    val method2 = cpg.method.name("libfunc").head

    cpg.id(method1.id).l shouldBe Seq(method1)
    cpg.id(Seq(method1.id, method2.id)).l shouldBe Seq(method1, method2)
  }

}
