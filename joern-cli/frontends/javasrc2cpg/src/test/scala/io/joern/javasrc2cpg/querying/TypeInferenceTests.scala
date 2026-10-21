package io.joern.javasrc2cpg.querying

import io.joern.javasrc2cpg.testfixtures.JavaSrcCode2CpgFixture
import io.joern.x2cpg.Defines
import io.shiftleft.codepropertygraph.generated.DispatchTypes
import io.shiftleft.codepropertygraph.generated.nodes.{Identifier, Literal}
import io.shiftleft.semanticcpg.language.*

import java.io.File

class NewTypeInferenceTests extends JavaSrcCode2CpgFixture {

  "a call with an unresolved argument with an ANY type" when {
    "there is only one matching method should be resolved" in {
      val cpg = code(
        """
          |package foo;
          |
          |import a.b.Bar;
          |
          |public class Foo {
          |  public static Bar test(String s) {
          |    return new Bar(s);
          |  }
          |
          |  public void sink(Bar b) {}
          |}
          |""".stripMargin,
        fileName = "Foo.java"
      ).moreCode("""
          |import foo.Foo;
          |
          |import baz.Baz;
          |
          |class Test {
          |  public static Bar stringCall() {
          |    sink(Foo.test(Baz.getString()));
          |  }
          |}
          |""".stripMargin)

      cpg.method.name("stringCall").call.name("test").methodFullName.l shouldBe List(
        "foo.Foo.test:a.b.Bar(java.lang.String)"
      )
    }

    "a call with an unresolved argument with a null type" when {
      "there is only one matching method should be resolved" in {
        val cpg = code(
          """
            |package foo;
            |
            |import a.b.Bar;
            |
            |public class Foo {
            |  public static Bar test(String s) {
            |    return new Bar(s);
            |  }
            |
            |  public void sink(Bar b) {}
            |}
            |""".stripMargin,
          fileName = "Foo.java"
        ).moreCode("""
            |import foo.Foo;
            |
            |import baz.Baz;
            |
            |class Test {
            |  public static Bar stringCall() {
            |    sink(Foo.test(null));
            |  }
            |}
            |""".stripMargin)

        cpg.method.name("stringCall").call.name("test").methodFullName.l shouldBe List(
          "foo.Foo.test:a.b.Bar(java.lang.String)"
        )
      }
    }

    "there are multiple matching methods should not be resolved" in {
      val cpg = code(
        """
          |package foo;
          |
          |import a.b.Bar;
          |
          |public class Foo {
          |  public static Bar test(String s) {
          |    return new Bar(s);
          |  }
          |
          |  public static Bar test(Integer i) {
          |    return new Bar(s);
          |  }
          |  public void sink(Bar b) {}
          |}
          |""".stripMargin,
        fileName = "Foo.java"
      ).moreCode("""
          |import foo.Foo;
          |
          |import baz.Baz;
          |
          |class Test {
          |  public static Bar stringCall() {
          |    sink(Foo.test(Baz.getString()));
          |  }
          |}
          |""".stripMargin)

      cpg.method.name("stringCall").call.name("test").methodFullName.l shouldBe List(
        "foo.Foo.test:<unresolvedSignature>(1)"
      )
    }
  }

  "methodFullNames for unresolved methods in source" should {
    val cpg = code(
      """
        |package org.codeminers.controller;
        |
        |import org.codeminers.thirdparty.ThirdParty;
        |
        |public class Controller {
        |
        |    public void foo() {
        |        Request request = new Request();
        |        ThirdParty.getSgClient().api(request);
        |    }
        |}""".stripMargin,
      fileName = "Controller.java"
    ).moreCode("""
        |package org.codeminers.thirdparty;
        |
        |import com.sendgrid.SendGrid;
        |
        |public class ThirdParty {
        |    public static SendGrid getSgClient() {
        |	     return new SendGrid("Dummy-api-key");
        |    }
        |}""".stripMargin)

    "should correctly infer the return type for getSgClient" in {
      // This is the simple case that can be solved with just import information.
      val List(method) = cpg.typeDecl.name("ThirdParty").method.name("getSgClient").l
      method.methodReturn.typeFullName shouldBe "com.sendgrid.SendGrid"
      method.fullName shouldBe "org.codeminers.thirdparty.ThirdParty.getSgClient:com.sendgrid.SendGrid()"
    }

    "have the correct signature if the method parameter and return types can be inferred" in {
      // This is the more complex case that relies on type information across compilation units.
      val methodFullName = cpg.call.name("getSgClient").head.methodFullName
      methodFullName shouldBe "org.codeminers.thirdparty.ThirdParty.getSgClient:com.sendgrid.SendGrid()"
    }
  }

  "method names should be inferred correctly based on call argument type information" in {
    val cpg = code(
      """
      |package foo;
      |
      |import a.b.Bar;
      |
      |public class Foo {
      |  public static Bar test(String s) {
      |    return new Bar(s);
      |  }
      |
      |  public static Bar test(Integer i) {
      |    return new Bar(s);
      |  }
      |  public void sink(Bar b) {}
      |}
      |""".stripMargin,
      fileName = "Foo.java"
    ).moreCode("""
      |import foo.Foo;
      |
      |class Test {
      |  public static Bar stringCall(String s) {
      |    sink(Foo.test(s));
      |  }
      |
      |  public static Bar intCall(Integer i) {
      |    sink(Foo.test(i));
      |  }
      |}
      |""".stripMargin)

    cpg.method.name("stringCall").call.name("test").methodFullName.l shouldBe List(
      "foo.Foo.test:a.b.Bar(java.lang.String)"
    )
    cpg.method.name("intCall").call.name("test").methodFullName.l shouldBe List(
      "foo.Foo.test:a.b.Bar(java.lang.Integer)"
    )
  }

  "type information for constructor invocations" should {

    "be found for constructor invocations at the start of a call chain" in {
      val cpg = code("""
          |import a.Bar;
          |
          |public class Foo {
          |  public void foo() {
          |    String s = new Bar().getValue();
          |  }
          |}
          |""".stripMargin)

      inside(cpg.call.nameExact(io.joern.x2cpg.Defines.ConstructorMethodName).methodFullName.l) { case List(fullName) =>
        fullName shouldBe s"a.Bar.${io.joern.x2cpg.Defines.ConstructorMethodName}:void()"
      }

      inside(cpg.call.nameExact("getValue").methodFullName.l) { case List(fullName) =>
        fullName shouldBe s"a.Bar.getValue:${Defines.UnresolvedSignature}(0)"
      }
    }

    "be found for constructor invocations as arguments" in {
      val cpg = code("""
          |import a.Bar;
          |
          |public class Foo {
          |
          |  public static void foo() {
          |    useBar(new Bar());
          |  }
          |
          |  public static void useBar(Bar b) {}
          |}
          |""".stripMargin)

      inside(cpg.call.nameExact(io.joern.x2cpg.Defines.ConstructorMethodName).methodFullName.l) { case List(fullName) =>
        fullName shouldBe s"a.Bar.${io.joern.x2cpg.Defines.ConstructorMethodName}:void()"
      }
    }

    "be found for constructor invocations as return args" in {
      val cpg = code("""
          |import a.Bar;
          |
          |public class Foo {
          |  public Bar getBar() {
          |    return new Bar();
          |  }
          |}
          |""".stripMargin)

      inside(cpg.call.nameExact(io.joern.x2cpg.Defines.ConstructorMethodName).methodFullName.l) { case List(fullName) =>
        fullName shouldBe s"a.Bar.${io.joern.x2cpg.Defines.ConstructorMethodName}:void()"
      }
    }
  }

  "type information for members" should {
    val cpg = code("""
        |import a.Logger;
        |import a.LoggerFactory;
        |import b.Environment;
        |
        |public class Foo {
        |  Environment env;
        |  private static Logger log = LoggerFactory.getLogger(Foo.class);
        |
        |  public void foo() {
        |    log.info("UserName is {}", env.getProperty("property"));
        |  }
        |}
        |""".stripMargin)

    "be inferred from imports" in {
      cpg.member.name("env").typeFullName.head shouldBe "b.Environment"
      cpg.member.name("log").typeFullName.head shouldBe "a.Logger"
    }

    "be used in calls" in {
      inside(cpg.call.name("info").methodFullName.l) { case List(fullName) =>
        fullName shouldBe s"a.Logger.info:${Defines.UnresolvedSignature}(2)"
      }

      inside(cpg.call.name("getProperty").methodFullName.l) { case List(fullName) =>
        fullName shouldBe s"b.Environment.getProperty:${Defines.UnresolvedSignature}(1)"
      }
    }
  }

  "constructors created from import info" should {
    val cpg = code("""
        |import a.b.c.Bar;
        |
        |class Foo {
        |  public void test2() {
        |    // Should create constructor for a.b.c.Bar
        |    Bar b = new Bar(0);
        |  }
        |}
        |""".stripMargin)

    "create a constructor based on import info" in {
      val init = inside(cpg.method.name("test2").call.nameExact(io.joern.x2cpg.Defines.ConstructorMethodName).l) {
        case init :: Nil => init
      }

      init.typeFullName shouldBe "void"
      init.signature shouldBe s"${Defines.UnresolvedSignature}(1)"
      init.methodFullName shouldBe s"a.b.c.Bar.${Defines.ConstructorMethodName}:${Defines.UnresolvedSignature}(1)"

      init.argument.size shouldBe 2

      inside(init.argument.l) { case List(obj: Identifier, arg: Literal) =>
        obj.name shouldBe "b"
        obj.code shouldBe "b"
        obj.typeFullName shouldBe "a.b.c.Bar"
        obj.order shouldBe 1
        obj.argumentIndex shouldBe 0

        arg.typeFullName shouldBe "int"
        arg.code shouldBe "0"
        arg.order shouldBe 2
        arg.argumentIndex shouldBe 1
      }
    }
  }

}

class JavaTypeRecoveryPassTests extends JavaSrcCode2CpgFixture(enableTypeRecovery = true) {

  "chained calls from external dependencies" should {
    lazy val cpg = code(
      """
        |package net.javaguides.hibernate;
        |
        |import java.util.List;
        |
        |import org.hibernate.Session;
        |import org.hibernate.Transaction;
        |
        |import net.javaguides.hibernate.entity.Student;
        |import net.javaguides.hibernate.util.HibernateUtil;
        |
        |public class NamedQueryExample {
        |	public static void main(String[] args) {
        |		saveStudent();
        |
        |		Transaction transaction = null;
        |		try (Session session = HibernateUtil.getSessionFactory().openSession()) {
        |			transaction = session.beginTransaction();
        |			// Executing named queries
        |
        |			List<Long> totalStudents = session.createNamedQuery("GET_STUDENTS_COUNT", Long.class).getResultList();
        |			System.out.println("Total Students: " + totalStudents.get(0));
        |
        |			transaction.commit();
        |		} catch (Exception e) {
        |			if (transaction != null) {
        |				transaction.rollback();
        |			}
        |		}
        |
        |	}
        |}
        |""".stripMargin,
      Seq("net", "javaguides", "hibernate", "NamedQueryExample.java").mkString(File.separator)
    )

    "should be resolved using dummy return values" in {
      val Some(getResultList) = cpg.call("getResultList").headOption: @unchecked
      // Changes the below from <unresolvedNamespace>.getResultList:<unresolvedSignature>(0) to:
      getResultList.methodFullName shouldBe "org.hibernate.Session.createNamedQuery:<unresolvedSignature>(2).<returnValue>.getResultList:<unresolvedSignature>(0)"
      getResultList.dynamicTypeHintFullName shouldBe Seq()
    }

    "hint that `transaction` may be of the null type" in {
      val Some(transaction) = cpg.identifier("transaction").headOption: @unchecked
      transaction.typeFullName shouldBe "org.hibernate.Transaction"
      transaction.dynamicTypeHintFullName.contains("null")
    }
  }

}

class TypeInferenceTests extends JavaSrcCode2CpgFixture {

  val cpg = code("""
      |package pakfoo;
      |
      |import a.b.c.Bar;
      |import d.*;
      |import e.Unknown;
      |
      |class Foo extends Unknown {
      |
      |    public static void foo(int x) {}
      |
      |    public void test1() {
      |        // Should find a.b.c.Bar
      |        Bar b;
      |    }
      |
      |    public void test2() {
      |        // Should create constructor for a.b.c.Bar
      |        Bar b = new Bar(0);
      |    }
      |
      |    // Should find a.b.c.Bar for parameter type
      |    public void test3(Bar b) {}
      |
      |    public void test4(Bar b) {
      |        // Should find methodFullName a.b.c.Bar.bar:int()
      |        int x = b.bar();
      |    }
      |
      |    public void test6(Baz z) {}
      |
      |    public void test7(Bar b, Baz z) {
      |        // Should find methodFullName a.b.c.Bar.bar:void(d.Baz, int)
      |        b.bar(z, 1);
      |    }
      |
      |    public void test8() {
      |        // Should find methodFullName pakfoo.Foo.missing:void()
      |        this.missing();
      |    }
      |
      |    public void test9() {
      |        // Should find methodFullName e.Unknown.missing:void()
      |        super.missing();
      |    }
      |
      |    public void test10() {
      |        // Should find arg type
      |        Foo f = new Foo();
      |    }
      |}
      |""".stripMargin)

  "should find typeFullName from matching import" in {
    inside(cpg.method.name("test1").local.nameExact("b").l) { case b :: Nil =>
      b.typeFullName shouldBe "a.b.c.Bar"
    }
  }

  "should find typeFullName for param from import" in {
    inside(cpg.method.name("test3").parameter.index(1).l) { case param :: Nil =>
      param.name shouldBe "b"
      param.typeFullName shouldBe "a.b.c.Bar"
    }
  }

  "should find methodFullName for unresolved call in assignment" in {
    val call = inside(cpg.method.name("test4").call.name("bar").l) { case call :: Nil =>
      call
    }

    call.typeFullName shouldBe "int"
    call.methodFullName shouldBe s"a.b.c.Bar.bar:${Defines.UnresolvedSignature}(0)"
    call.signature shouldBe s"${Defines.UnresolvedSignature}(0)"

    inside(call.argument.l) { case (obj: Identifier) :: Nil =>
      obj.name shouldBe "b"
      obj.code shouldBe "b"
      obj.typeFullName shouldBe "a.b.c.Bar"
    }
  }

  "should find typeFullName for unresolved param from single wildcard import" in {
    inside(cpg.method.name("test6").parameter.l) { case List(_, bazParam) =>
      bazParam.name shouldBe "z"
      bazParam.typeFullName shouldBe "d.Baz"
    }
  }

  "should find methodFullName for unresolved call with unresolved argument" in {
    val call = inside(cpg.method.name("test7").call.name("bar").l) { case call :: Nil =>
      call
    }

    call.typeFullName shouldBe "void"
    call.signature shouldBe s"${Defines.UnresolvedSignature}(2)"
    call.methodFullName shouldBe s"a.b.c.Bar.bar:${Defines.UnresolvedSignature}(2)"

    inside(call.argument.l) { case List(obj: Identifier, arg1: Identifier, arg2: Literal) =>
      obj.name shouldBe "b"
      obj.typeFullName shouldBe "a.b.c.Bar"

      arg1.name shouldBe "z"
      arg1.typeFullName shouldBe "d.Baz"

      arg2.code shouldBe "1"
      arg2.typeFullName shouldBe "int"
    }
  }

  "should guess the enclosing typeDecl type for unresolved explicit this calls" in {
    val call = inside(cpg.method.name("test8").call.name("missing").l) { case call :: Nil =>
      call
    }

    call.typeFullName shouldBe "void"
    call.signature shouldBe s"${Defines.UnresolvedSignature}(0)"
    call.methodFullName shouldBe s"pakfoo.Foo.missing:${Defines.UnresolvedSignature}(0)"
    call.dispatchType shouldBe DispatchTypes.DYNAMIC_DISPATCH

    inside(call.argument.l) { case (obj: Identifier) :: Nil =>
      obj.name shouldBe "this"
      obj.order shouldBe 1
      obj.argumentIndex shouldBe 0
      obj.typeFullName shouldBe "pakfoo.Foo"
    }
  }

  "should find type and methodFullName for unresolved super call" in {
    pendingUntilFixed {

      val call = inside(cpg.method.name("test9").call.name("missing").l) { case call :: Nil =>
        call
      }

      call.typeFullName shouldBe "void"
      call.signature shouldBe "void()"
      call.methodFullName shouldBe "e.Unknown.missing:void()"
      call.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH

      inside(call.argument.l) { case (obj: Identifier) :: Nil =>
        obj.name shouldBe "super"
        obj.order shouldBe 1
        obj.argumentIndex shouldBe 0
        obj.typeFullName shouldBe "e.Unknown"
      }
    }
  }

}
