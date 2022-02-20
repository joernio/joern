package io.joern.c2cpg.querying

import io.joern.c2cpg.testfixtures.CCodeToCpgSuite
import io.shiftleft.codepropertygraph.generated.{Languages, NodeTypes}
import io.shiftleft.semanticcpg.language._
import overflowdb.traversal._

/** The following tests show in detail how queries can be started. For all node types, for which it seems reasonable,
  * all nodes of that type can be used as a starting point, e.g., `cpg.method` starts at all methods while `cpg.local`
  * starts at all locals.
  */
class NodeTypeStartersTests extends CCodeToCpgSuite {

  override val code = """
       /* A C comment */
       // A C++ comment
       int main(int argc, char **argv) { int mylocal; libfunc(1, argc); }
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
    cpg.method.internal.name.l shouldBe List("<global>", "main")
    cpg.method.external.name.l shouldBe List("libfunc")
  }

  "should allow retrieving comments" in {
    cpg.comment.code.toSetMutable shouldBe Set("/* A C comment */", "// A C++ comment")
  }

  "should allow retrieving parameters" in {
    cpg.parameter.where(_.method.internal).name.toSetMutable shouldBe Set("argc", "argv")
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
    cpg.typeDecl.nameNot("<global>").internal.name.toSetMutable shouldBe Set("foo")
  }

  "should allow retrieving members" in {
    cpg.member.name.l shouldBe List("x")
  }

  "should allow retrieving (used) types" in {
    cpg.typ.name.toSetMutable shouldBe Set("int", "void", "char**", "ANY", "foo")
  }

  "should allow retrieving namespaces" in {
    cpg.namespace.name.l shouldBe List("<global>")
  }

  "should allow retrieving namespace blocks" in {
    cpg.namespaceBlock.name.toSetMutable shouldBe Set("<global>")
  }

  "should allow retrieving of method returns" in {
    cpg.methodReturn.l.size shouldBe 3
  }

  "should allow retrieving of meta data" in {
    cpg.metaData.language.l shouldBe List(Languages.NEWC)
  }

  "should allow retrieving all nodes" in {
    val allNodesLabels = cpg.all.label.toSetMutable

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
      NodeTypes.LITERAL,
      NodeTypes.BINDING,
      NodeTypes.IDENTIFIER
    )
  }

  "should allow retrieving nodes by id" in {
    val method1 = cpg.method.name("main").head
    val method2 = cpg.method.name("libfunc").head

    cpg.id(method1.id).l shouldBe Seq(method1)
    cpg.id(Seq(method1.id, method2.id)).l shouldBe Seq(method1, method2)
  }

}
