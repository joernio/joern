package io.joern.jimple2cpg.querying

import io.joern.jimple2cpg.testfixtures.JimpleCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.semanticcpg.language._

/** Right now reflection is mostly unsupported. This should be extended in later when it is.
  */
class ReflectionTests extends JimpleCode2CpgFixture {

  val cpg: Cpg = code("""
      |class Foo {
      | static int add(int x, int y) {
      |   return x + y;
      | }
      |
      | static void foo() throws NoSuchMethodException {
      |   var fooClazz = Foo.class;
      |   var fooMethod = fooClazz.getMethod("add", int.class, int.class);
      | }
      |}
      |""".stripMargin).cpg

  "should assign the class and method variables correctly" in {
    val List(fooClazz, fooMethod) = cpg.method("foo").ast.isIdentifier.name("fooClazz", "fooMethod").take(2).l

    fooClazz.typeFullName shouldBe "java.lang.Class"
    fooClazz.parentExpression match {
      case Some(call) =>
        call.ast.isLiteral.headOption match {
          case Some(classLiteral) =>
            classLiteral.code shouldBe "Foo.class"
            classLiteral.typeFullName shouldBe "java.lang.Class"
          case None => fail("Should be assigned to a class literal")
        }
      case None => fail("Should be the child of an <operator>.assignment call")
    }
    fooMethod.typeFullName shouldBe "java.lang.reflect.Method"
  }
}
