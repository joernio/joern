package io.joern.php2cpg.passes

import io.joern.php2cpg.testfixtures.PhpCode2CpgFixture
import io.shiftleft.semanticcpg.language._

class PhpTypeRecoveryPassTests extends PhpCode2CpgFixture() {

  /* TODO: Future tests to specify correct type recovery behaviors:
   * - Method call inherited from a super class should be recovered
   * - A type hint on a parameter should be sufficient to resolve method full names at calls
   * - Parameter types on builtins with variadic parameters
   */

  "literals declared from built-in types" should {
    lazy val cpg = code("""
      |<?php
      |function foo_return_int() {
      |   $x = 1;
      |   return $x;
      |}
      |""".stripMargin).cpg

    "resolve 'x' identifier type" in {
      val List(xIdentifier) = cpg.identifier("x").take(1).l
      xIdentifier.typeFullName shouldBe "int"
    }
    "resolve 'foo_return_int()' return value" in {
      val List(fooMethod) = cpg.method("foo_return_int").take(1).l
      fooMethod.methodReturn.dynamicTypeHintFullName shouldBe Seq("int")
    }
  }

  /* These tests are written assuming a context sensitive type recovery pass,
   * but the current implementation is context insenstive.
   * Test is set to be ignored, but should be revisited when conext sensitivity
   * can be added.
   */
  "literals declared from built-in types that are shadowed" should {
    lazy val cpg = code("""
        |<?php
        |$x = 123;
        |
        |function foo_shadowing() {
        |   $x = "foo";
        |}
        |""".stripMargin).cpg

    "resolve 'x' identifier types despite shadowing" ignore {
      val List(xOuterScope, xInnerScope) = cpg.identifier("x").take(2).l
      xOuterScope.dynamicTypeHintFullName shouldBe Seq("int")
      xInnerScope.dynamicTypeHintFullName shouldBe Seq("string")
    }
  }

  "primitive types returned from built-in functions" should {
    lazy val cpg = code("""
      |<?php
      |function foo_return_strtolower() {
      |   return strtolower("hello");
      |}
      |""".stripMargin).cpg

    "resolve built-in methodReturn type" in {
      val List(strtolowerMethod) = cpg.method("strtolower").take(1).l
      strtolowerMethod.methodReturn.typeFullName shouldBe "string"
    }

    "resolve built-in parameter type" in {
      val List(strtolowerMethod)    = cpg.method("strtolower").take(1).l
      val List(strtolowerParameter) = strtolowerMethod.parameter.take(1).l
      strtolowerParameter.typeFullName shouldBe "string"
    }

    "resolve calling function methodReturn type" in {
      val List(fooMethod) = cpg.method("foo_return_strtolower").take(1).l
      fooMethod.methodReturn.dynamicTypeHintFullName shouldBe Seq("string")
    }
  }

  "class declaration" should {
    lazy val cpg = code("""
      |<?php
      |class ClassA {
      |   private $attrA = 0;
      |
      |   function bar() {
      |     return $this->attrA;
      |   }
      |}
      |function foo_instantiate_classA() {
      |   $a = new ClassA();
      |   return $a;
      |}
      |""".stripMargin).cpg

    "recover type of class member assigned to literal" in {
      val List(attrA) = cpg.typeDecl("ClassA").member.name("attrA").take(1).l
      attrA.typeFullName shouldBe "int"
    }

    "recover type of method that returns class member" in {
      val List(barMethod) = cpg.typeDecl("ClassA").method.name("bar").take(1).l
      barMethod.methodReturn.dynamicTypeHintFullName shouldBe Seq("int")
    }

    "recover type of object that instantiates a class" in {
      val List(aObject) = cpg.identifier("a").take(1).l
      aObject.typeFullName shouldBe "ClassA"
    }

    "recover type of function that returns object" in {
      val List(fooMethod) = cpg.method("foo_instantiate_classA").take(1).l
      fooMethod.methodReturn.dynamicTypeHintFullName shouldBe Seq("ClassA")
    }
  }

  "functions that return multiple objects" should {
    lazy val cpg = code("""
      |<?php
      |class ClassA {
      |   private $foo = 1;
      |
      |   function bar() {
      |     return $this->foo;
      |   }
      |}
      |class ClassB {
      |   private $foo = 0;
      |
      |   function baz() {
      |     return $this->foo;
      |   }
      |}
      |
      |function foo_return_different_objects($type_param) {
      |   if ($type_param == 0) {
      |     $a = new ClassA();
      |   } else {
      |     $a = new ClassB();
      |   }
      |   return $a;
      |}
      |""".stripMargin).cpg

    "recover both possible types for local variable" in {
      val List(aIdentifier) = cpg.identifier("a").take(1).l
      aIdentifier.dynamicTypeHintFullName shouldBe Seq("ClassA", "ClassB")
    }

    "recover both possible types for function return" in {
      val List(fooMethod) = cpg.method("foo_return_different_objects").take(1).l
      fooMethod.methodReturn.dynamicTypeHintFullName shouldBe Seq("ClassA", "ClassB")
    }
  }

  /* Joern's PHP front-end does not currently handle comments. This test is
   * ignored, but should be revisited when comments are handled.
   */
  "functions with docblock type information" should {
    lazy val cpg = code("""
      |<?php
      |/**
      | * @return int
      | */
      |function foo_unknown_param($param) {
      |   return $param;
      |}
      |""".stripMargin).cpg

    "identify function return from docblock" ignore {
      val List(fooMethod) = cpg.method("foo_unknown_param").take(1).l
      fooMethod.methodReturn.dynamicTypeHintFullName shouldBe Seq("int")
    }
  }

  "class members with getters and setters" should {
    lazy val cpg = code("""
      |<?php
      |class ClassA {
      |   private $foo;
      |
      |   function set_foo() {
      |     $this->foo = 1;
      |   }
      |
      |   function get_foo() {
      |     return $this->foo;
      |   }
      |}
      |""".stripMargin).cpg

    "identify class member type from setter" in {
      val List(fooMember) = cpg.typeDecl("ClassA").member.name("foo").take(1).l
      fooMember.typeFullName shouldBe "int"
    }

    "identify getter return type from class member" in {
      val List(getterMethod) = cpg.method("get_foo").take(1).l
      getterMethod.methodReturn.dynamicTypeHintFullName shouldBe Seq("int")
    }
  }

  "functions with multiple return statements with two different types" should {
    lazy val cpg = code("""
      |<?php
      |function foo_multiple_returns($param) {
      |   if ($param === 0) {
      |     return 1;
      |   } else {
      |     return "abc";
      |   }
      |}
      |""".stripMargin).cpg

    "identify both possible return types" in {
      val List(fooMethod) = cpg.method("foo_multiple_returns").take(1).l
      fooMethod.methodReturn.dynamicTypeHintFullName.toSet shouldBe Set("int", "string")
    }
  }

  "classes with static methods" should {
    lazy val cpg = code("""
      |<?php
      |class ClassA {
      |
      |   static function static_method() {
      |     return 5;
      |   }
      |
      |   function bar() {
      |     $tmp_bar = self::static_method();
      |     return $tmp_bar;
      |   }
      |}
      |
      |function foo_call_static_method() {
      |   $tmp_foo = ClassA::static_method();
      |   return ClassA::static_method();
      |}
      |""".stripMargin).cpg

    "identify return type of static method in declaration" in {
      val List(staticMethod) = cpg.method("static_method").take(1).l
      staticMethod.methodReturn.dynamicTypeHintFullName shouldBe Seq("int")
    }

    "identify return type of static method called outside of class" in {
      val List(fooMethod)     = cpg.method("foo_call_static_method").take(1).l
      val List(tmpIdentifier) = cpg.identifier("tmp_foo").take(1).l
      tmpIdentifier.typeFullName shouldBe "int"
      fooMethod.methodReturn.dynamicTypeHintFullName shouldBe Seq("int")
    }

    "identify return type of static method called using 'self' inside class" in {
      val List(barMethod)     = cpg.typeDecl("ClassA").method.name("bar").take(1).l
      val List(tmpIdentifier) = cpg.identifier("tmp_bar").take(1).l
      tmpIdentifier.typeFullName shouldBe "int"
      barMethod.methodReturn.dynamicTypeHintFullName shouldBe Seq("int")
    }
  }

  /* These tests are written assuming a context sensitive type recovery pass,
   * but the current implementation is context insenstive.
   * Test is set to be ignored, but should be revisited when conext sensitivity
   * can be added.
   */
  "modules with name conflicts in different local scopes" should {
    lazy val cpg = code("""
      |<?php
      |
      |function foo() {
      |   $local = 0;
      |   return $local;
      |}
      |
      |function bar() {
      |   $local = "hello";
      |   return $local;
      |}
      """.stripMargin).cpg

    "identify correct types for scoped local variables" ignore {
      val List(localFooIdentifier) = cpg.method("foo").ast.isIdentifier.name("local").take(1).l
      val List(localBarIdentifier) = cpg.method("bar").ast.isIdentifier.name("local").take(1).l
      localFooIdentifier.typeFullName shouldBe "int"
      localBarIdentifier.typeFullName shouldBe "string"
    }

    "identify correct types for function returns based on local variable types" ignore {
      val List(fooMethod) = cpg.method("foo").take(1).l
      val List(barMethod) = cpg.method("bar").take(1).l
      fooMethod.methodReturn.dynamicTypeHintFullName shouldBe Seq("int")
      barMethod.methodReturn.dynamicTypeHintFullName shouldBe Seq("string")
    }
  }

  "assignments to identified array fields" should {
    lazy val cpg = code("""
      |<?php
      |
      |function foo_set_array_field() {
      |   $arr = array();
      |   $arr["a"] = 1;
      |}
      |""")

    "recover type of array" in {
      val List(arrIdentifier) = cpg.identifier("arr").take(1).l
      arrIdentifier.typeFullName shouldBe "array"
    }

    /* Consider where in the CPG that array type info should be stored */
    "recover type of array field being assigned to" ignore {
      val List(indexAccessCall) = cpg.call("<operator>.indexAccess").take(1).l
      indexAccessCall.typeFullName shouldBe "int"
    }
  }

  "function declarations with type hints" should {
    lazy val cpg = code("""
    |<?php
    |function foo(string $foo_param): string {
    |   return $foo_param;
    |}
    |
    |function bar(int $bar_param): int {
    |   return $bar_param;
    |}
    |
    |function baz(string $param1, int $param2) {
    |   $tmp_foo = foo(param1);
    |   $tmp_bar = bar(param2);
    |}
    |""".stripMargin).cpg

    "set method return types from the type hints" in {
      val List(fooMethod) = cpg.method("foo").take(1).l
      val List(barMethod) = cpg.method("bar").take(1).l
      fooMethod.methodReturn.dynamicTypeHintFullName shouldBe Seq("string")
      barMethod.methodReturn.dynamicTypeHintFullName shouldBe Seq("int")
    }

    "set types of identifiers from assignments" in {
      val List(tmpFooIdentifier) = cpg.identifier("tmp_foo").take(1).l
      val List(tmpBarIdentifier) = cpg.identifier("tmp_bar").take(1).l
      tmpFooIdentifier.typeFullName shouldBe "string"
      tmpBarIdentifier.typeFullName shouldBe "int"
    }
  }

  "methods that are dynamically invoked" should {
    lazy val cpg = code("""
        |<?php
        |class ClassA {
        | function foo() {
        |   return 5;
        | }
        |
        | function bar() {
        |   return $this->foo();
        | }
        |}
        |
        |function baz() {
        | $a = new ClassA();
        | return $a->foo();
        |}
        |""".stripMargin).cpg

    "be properly resolved when called with $this" in {
      val List(fooCall) = cpg.method("bar").ast.isCall.take(1).l
      fooCall.methodFullName shouldBe "ClassA->foo"
    }

    "be properly resolved when called through class with known type" in {
      val List(fooCall) = cpg.method("baz").ast.isCall.filter(_.code == "$a->foo()").take(1).l
      fooCall.methodFullName shouldBe "ClassA->foo"
    }

    "propagate type information to calling method" in {
      val List(bazMethod) = cpg.method("baz").take(1).l
      bazMethod.methodReturn.dynamicTypeHintFullName shouldBe Seq("int")
    }

    "propagate type information to calling method when called with $this" in {
      val List(barMethod) = cpg.method("bar").take(1).l
      barMethod.methodReturn.dynamicTypeHintFullName shouldBe Seq("int")
    }
  }

  "modules that import modules" should {
    lazy val cpg = code(
      """
        |<?php
        |class ClassA {
        |   function foo() {
        |     return 5;
        |   }
        |}
        |""".stripMargin,
      "classA.php"
    ).moreCode(
      """
        |<?php
        |include 'classA.php';
        |
        |function bar() {
        |   $a = new ClassA();
        |   return $a->foo();
        |}
        |""".stripMargin,
      "useA.php"
    )

    "recover the type of object instantiated from imported module class" in {
      val List(aIdentifier) = cpg.identifier("a").take(1).l
      aIdentifier.typeFullName shouldBe "ClassA"
    }

    "recover method return value assigned from class method" in {
      val List(barMethod) = cpg.method("bar").take(1).l
      barMethod.methodReturn.dynamicTypeHintFullName shouldBe Seq("int")
    }
  }
}
