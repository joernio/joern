package io.joern.c2cpg.cpp.features20

import io.joern.c2cpg.astcreation.Defines
import io.joern.c2cpg.parser.FileDefaults
import io.joern.c2cpg.testfixtures.AstC2CpgSuite
import io.shiftleft.codepropertygraph.generated.ControlStructureTypes
import io.shiftleft.semanticcpg.language.*

class Cpp20FeaturesTests extends AstC2CpgSuite(fileSuffix = FileDefaults.CppExt) {

  "C++20 feature support" should {

    "handle coroutines" in {
      val cpg = code("""
          |generator<int> range(int start, int end) {
          |  while (start < end) {
          |    co_yield start;
          |    start++;
          |  }
          |}
          |
          |task<void> echo(socket s) {
          |  for (;;) {
          |    auto data = co_await s.async_read();
          |    co_await async_write(s, data);
          |  }
          |}
          |""".stripMargin)
      cpg.method
        .nameExact("range")
        .controlStructure
        .astChildren
        .isBlock
        .astChildren
        .isCall
        .nameExact("<operator>.yield")
        .size shouldBe 1

      pendingUntilFixed {
        // `auto data = co_await s.async_read();` can not be parsed yet.
        // Hence, this co_await call is missing.
        cpg.method
          .nameExact("echo")
          .controlStructure
          .astChildren
          .isBlock
          .astChildren
          .isCall
          .nameExact("<operator>.await")
          .size shouldBe 2
      }
    }

    "handle concepts" in {
      val cpg = code("""
          |// `T` is not limited by any constraints.
          |template <typename T>
          |concept always_satisfied = true;
          |// Limit `T` to integrals.
          |template <typename T>
          |concept integral = std::is_integral_v<T>;
          |// Limit `T` to both the `integral` constraint and signedness.
          |template <typename T>
          |concept signed_integral = integral<T> && std::is_signed_v<T>;
          |// Limit `T` to both the `integral` constraint and the negation of the `signed_integral` constraint.
          |template <typename T>
          |concept unsigned_integral = integral<T> && !signed_integral<T>;
          |
          |// Forms for function parameters:
          |// `T` is a constrained type template parameter.
          |template <my_concept T>
          |void f1(T v);
          |
          |// `T` is a constrained type template parameter.
          |template <typename T>
          |  requires my_concept<T>
          |void f2(T v);
          |
          |// `T` is a constrained type template parameter.
          |template <typename T>
          |void f3(T v) requires my_concept<T>;
          |
          |// `v` is a constrained deduced parameter.
          |void f4(my_concept auto v);
          |
          |// `v` is a constrained non-type template parameter.
          |template <my_concept auto v>
          |void f5();
          |
          |void foo() {
          |  // Forms for lambdas:
          |  // `T` is a constrained type template parameter.
          |  auto l1 = []<my_concept T> (T v) {
          |    // ...
          |  };
          |  // `T` is a constrained type template parameter.
          |  auto l2 = []<typename T> requires my_concept<T> (T v) {
          |    // ...
          |  };
          |  // `T` is a constrained type template parameter.
          |  auto l3 = []<typename T> (T v) requires my_concept<T> {
          |    // ...
          |  };
          |  // `v` is a constrained deduced parameter.
          |  auto l4 = [](my_concept auto v) {
          |    // ...
          |  };
          |  // `v` is a constrained non-type template parameter.
          |  auto l5 = []<my_concept auto v> () {
          |    // ...
          |  };
          |}
          |
          |template <typename T>
          |  requires my_concept<T> // `requires` clause.
          |void f6(T);
          |
          |template <typename T>
          |concept callable = requires (T f) { f(); }; // `requires` expression.
          |
          |template <typename T>
          |  requires requires (T x) { x + x; } // `requires` clause and expression on same line.
          |T add(T a, T b) {
          |  return a + b;
          |}
          |
          |template <typename T>
          |concept C1 = requires(T x) {
          |  {*x} -> std::convertible_to<typename T::inner>; // the type of the expression `*x` is convertible to `T::inner`
          |  {x + 1} -> std::same_as<int>; // the expression `x + 1` satisfies `std::same_as<decltype((x + 1))>`
          |  {x * 1} -> std::convertible_to<T>; // the type of the expression `x * 1` is convertible to `T`
          |};
          |
          |template <typename T>
          |concept C2 = requires(T x) {
          |  requires std::same_as<sizeof(x), size_t>;
          |};
          |""".stripMargin)
      // we can't express concepts withing the CPG but parsing constructs using them should not be hindered
      // sadly, at some places (e.g., for parameters) it fails parsing
      cpg.method.nameNot("<global>").fullName.sorted.l shouldBe List(
        "add:T(T,T)",
        "f1:void(T)",
        "f2:void(T)",
        "f3:void(T)",
        "f6:void(T)",
        "foo:void()",
        "requires:requires(T)"
      )
      pendingUntilFixed {
        cpg.method.nameNot("<global>").fullName.sorted.l shouldBe List(
          "add:T(T,T)",
          "f1:void(T)",
          "f2:void(T)",
          "f3:void(T)",
          "f4:void(ANY)",
          "f5:void()",
          "f6:void(T)",
          // remaining lambdas ...
          "foo:void()"
        )
      }
    }

    "handle three-way comparison" in {
      val cpg = code("""
          |bool foo() {
          |  bool x = 1 <=> 2;
          |  return x;
          |}
          |""".stripMargin)
      // three-way comparison operator can not be parsed at the moment
      pendingUntilFixed {
        cpg.assignment.code.l shouldBe List("bool x = 1 <=> 2")
        // TODO: test for children AST elements
      }
    }

    "handle designated initializers" in {
      val cpg = code("""
          |struct A {
          |  int x;
          |  int y;
          |  int z = 123;
          |};
          |
          |void foo() {
          |  A a {.x = 1, .z = 2}; // a.x == 1, a.y == 0, a.z == 2
          |}
          |""".stripMargin)
      cpg.assignment.code.sorted.l shouldBe List("a.x = 1", "a.z = 2", "z = 123")
      pendingUntilFixed {
        // CDT failed to assign the type A to a here so we are not
        // able to identify all struct fields. Hence, no a.y = 0 assignment yet.
        cpg.assignment.code.sorted.l shouldBe List("a.x = 1", "a.y = 0", "a.z = 2", "z = 123")
      }
    }

    "handle template syntax for lambdas" in {
      val cpg = code("""
          |void foo() {
          |  auto f = []<typename T>(std::vector<T> v) {
          |    // ...
          |  };
          |}
          |""".stripMargin)
      cpg.method.nameNot("<global>").fullName.sorted.l shouldBe List("foo:void()")
      pendingUntilFixed {
        // []<typename T>(std::vector<T> v) { ... } can not be parsed by CDT at the moment
        cpg.method.nameNot("<global>").fullName.sorted.l shouldBe List("foo:void()", "<lambda>0")
      }
    }

    "handle range-based for loop with initializer" in {
      val cpg = code("""
          |void foo() {
          |  for (auto v = list; auto& e : v) {
          |    std::cout << e;
          |  }
          |}
          |""".stripMargin)
      pendingUntilFixed {
        // range-based for loop with initializer can not be parsed at all by CDT at the moment
        val List(v, e) = cpg.method.nameExact("foo").controlStructure.astChildren.isLocal.l
        v.name shouldBe "v"
        e.name shouldBe "e"
        val List(vectorInitCall) =
          cpg.method.nameExact("foo").controlStructure.astChildren.order(2).isCall.argument.isCall.l
        vectorInitCall.argumentIndex shouldBe 2
        vectorInitCall.name shouldBe Defines.OperatorConstructorInitializer
        vectorInitCall.argument.code.l shouldBe List("1", "2", "3")
      }
    }

    "handle likely and unlikely attributes" in {
      val cpg = code("""
          |void foo() {
          |  switch (n) {
          |    case 1:
          |      case1();
          |      break;
          |    [[likely]] case 2:  // n == 2 is considered to be arbitrarily more
          |      case2()           // likely than any other value of n
          |      break;
          |  }
          |
          |  int random = get_random_number_between_x_and_y(0, 3);
          |  if (random > 0) [[likely]] {
          |    // body of if statement
          |    likelyIf();
          |  }
          |
          |  while (unlikely_truthy_condition) [[unlikely]] {
          |    // body of while statement
          |    unlikelyWhile()
          |  }
          |}
          |""".stripMargin)
      val cases =
        cpg.method
          .nameExact("foo")
          .controlStructure
          .controlStructureTypeExact(ControlStructureTypes.SWITCH)
          .astChildren
          .isBlock
          ._jumpTargetViaAstOut
      cases.code.l shouldBe List("case 1:", "[[likely]] case 2:")
      cpg.method
        .nameExact("foo")
        .controlStructure
        .controlStructureTypeExact(ControlStructureTypes.SWITCH)
        .ast
        .isCall
        .code
        .l shouldBe List("case1()", "case2()")

      cpg.method
        .nameExact("foo")
        .controlStructure
        .controlStructureTypeExact(ControlStructureTypes.IF)
        .ast
        .isCall
        .code
        .l shouldBe List("random > 0", "likelyIf()")

      cpg.method
        .nameExact("foo")
        .controlStructure
        .controlStructureTypeExact(ControlStructureTypes.WHILE)
        .ast
        .isCall
        .code
        .l shouldBe List("unlikelyWhile()")
    }

    "handle deprecate implicit capture of this" in {
      val cpg = code("""
          |struct int_value {
          |  int n = 0;
          |  auto getter_fn() {
          |    return [=, *this]() { return n; };
          |  }
          |};
          |""".stripMargin)
      // TODO: we can not express these lambda types in the current schema
      // We would need to add a new type for lambdas that capture `this`
      cpg.method.nameExact("getter_fn").methodReturn.typeFullName.l shouldBe List(Defines.Function)
    }

    "handle class types in non-type template parameters" in {
      val cpg = code("""
          |struct foo {
          |  foo() = default;
          |  constexpr foo(int) {}
          |};
          |
          |template <foo f = {}>
          |auto get_foo() {
          |  return f;
          |}
          |
          |void main() {
          |  get_foo(); // uses implicit constructor
          |  get_foo<foo{123}>();
          |}
          |""".stripMargin)
      cpg.typeDecl.nameExact("foo").size shouldBe 1
      cpg.method.nameExact("get_foo").size shouldBe 1
      cpg.method.nameExact("main").ast.isCall.typeFullName.l shouldBe List("ANY", "foo")
      cpg.method.nameExact("main").ast.isCall.methodFullName.l shouldBe List(
        // we can not resolve the implicit constructor call case:
        "<unresolvedNamespace>.get_foo:<unresolvedSignature>(0)",
        "get_foo:foo()"
      )
      pendingUntilFixed {
        cpg.method.nameExact("main").ast.isCall.typeFullName.l shouldBe List("foo", "foo")
        cpg.method.nameExact("main").ast.isCall.methodFullName.l shouldBe List("get_foo:foo()", "get_foo:foo()")
      }
    }

    "handle constexpr virtual functions" in {
      val cpg = code("""
          |struct X1 {
          |  virtual int f() const = 0;
          |};
          |
          |struct X2: public X1 {
          |  constexpr virtual int f() const { return 2; }
          |};
          |
          |struct X3: public X2 {
          |  virtual int f() const { return 3; }
          |};
          |
          |struct X4: public X3 {
          |  constexpr virtual int f() const { return 4; }
          |};
          |
          |void foo() {
          |  constexpr X4 x4;
          |  x4.f(); // == 4
          |}
          |""".stripMargin)
      cpg.method.nameNot("<global>").fullName.sorted.l shouldBe List(
        "X1.f<const>:int()",
        "X2.f<const>:int()",
        "X3.f<const>:int()",
        "X4.f<const>:int()",
        "foo:void()"
      )
      cpg.method.nameExact("foo").local.typeFullName.l shouldBe List("X4")
    }

    "handle explicit(bool)" in {
      val cpg = code("""
          |struct foo {
          |  // Specify non-integral types (strings, floats, etc.) require explicit construction.
          |  template <typename T>
          |  explicit(!std::is_integral_v<T>) foo(T) {}
          |};
          |
          |void foo() {
          |  foo a = 123;
          |  foo c {"123"};
          |}
          |""".stripMargin)
      cpg.method.nameExact("foo").local.typeFullName.distinct.l shouldBe List("foo")
      pendingUntilFixed {
        // CDT can not parse explicit(bool) yet
        cpg.method.nameNot("<global>").fullName.sorted.l shouldBe List("A.foo:T(T)", "foo:void()")
      }
    }

    "handle immediate functions" in {
      val cpg = code("""
          |consteval int sqr(int n) {
          |  return n * n;
          |}
          |
          |void foo() {
          |  constexpr int r = sqr(100);
          |}
          |""".stripMargin)
      cpg.method.nameNot("<global>").fullName.sorted.l shouldBe List("foo:void()", "sqr:int(int)")
      val List(rLocal) = cpg.method.nameExact("foo").local.l
      rLocal.typeFullName shouldBe "int"
      rLocal.code shouldBe "constexpr int r"
    }

    "handle using enum" in {
      val cpg = code("""
          |enum class rgba_color_channel { red, green, blue, alpha };
          |
          |std::string_view to_string(rgba_color_channel my_channel) {
          |  switch (my_channel) {
          |    using enum rgba_color_channel;
          |    case red:   return "red";
          |    case green: return "green";
          |    case blue:  return "blue";
          |    case alpha: return "alpha";
          |  }
          |}
          |""".stripMargin)
      val List(switchBlock) = cpg.method
        .nameExact("to_string")
        .controlStructure
        .controlStructureTypeExact(ControlStructureTypes.SWITCH)
        .astChildren
        .isBlock
        .l
      switchBlock._jumpTargetViaAstOut.code.l shouldBe List("case red:", "case green:", "case blue:", "case alpha:")
      pendingUntilFixed {
        // using clause is not parsed by CDT at all yet
        switchBlock._jumpTargetViaAstOut.astChildren.isCall.code.l shouldBe List(
          "rgba_color_channel.red",
          "rgba_color_channel.green",
          "rgba_color_channel.blue",
          "rgba_color_channel.alpha"
        )
      }
    }

    "handle lambda capture of parameter pack" in {
      val cpg = code("""
          |template <typename... Args>
          |auto f1(Args&&... args){
          |    // BY VALUE:
          |    return [...args = std::forward<Args>(args)] {};
          |}
          |
          |template <typename... Args>
          |auto f2(Args&&... args){
          |    // BY REFERENCE:
          |    return [&...args = std::forward<Args>(args)] {};
          |}
          |""".stripMargin)
      cpg.method.nameNot("<global>").fullName.sorted.l shouldBe List("f1:ANY(Args&&)", "f2:ANY(Args&&)")
      cpg.method.nameNot("<global>").signature.sorted.l shouldBe List("ANY(Args&&)", "ANY(Args&&)")
      pendingUntilFixed {
        // the actual return value (i.e., the lambda defined at the return) can not be parsed by CDT
        cpg.method.nameExact("f1").ast.isReturn.astChildren.isMethodRef.code.l shouldBe List(
          "[...args = std::forward<Args>(args)] {};"
        )
        cpg.method.nameExact("f2").ast.isReturn.astChildren.isMethodRef.code.l shouldBe List(
          "[&...args = std::forward<Args>(args)] {};"
        )
      }
    }

    "handle char8_t" in {
      val cpg = code("""
          |char8_t utf8_str[] = u8"\u0123";
          |""".stripMargin)
      val List(assignmentCall) = cpg.call.l
      assignmentCall.code shouldBe """utf8_str[] = u8"\u0123""""
      val List(utf8_str) = assignmentCall.argument.isIdentifier.l
      utf8_str.name shouldBe "utf8_str"
      utf8_str.typeFullName shouldBe "char8_t[6]"
      val List(u8) = assignmentCall.argument.isLiteral.l
      u8.code shouldBe """u8"\u0123""""
      u8.typeFullName shouldBe "char[6]"
    }

    "handle constinit" in {
      val cpg = code("""
          |constexpr const char* f(bool p) { return p ? "constant initializer" : g(); }
          |
          |void foo() {
          |  constinit const char *c = f(true);
          |}
          |""".stripMargin)
      cpg.method.nameNot("<global>").fullName.sorted.l shouldBe List("f:char*(bool)", "foo:void()")
      val List(cLocal) = cpg.method.nameExact("foo").local.l
      cLocal.typeFullName shouldBe "char*"
      cLocal.code shouldBe "const char *c" // constinit keyword is not parsed by CDT
    }

    "handle __VA_OPT__" in {
      val cpg = code("""
          |#define F(...) f(0 __VA_OPT__(,) __VA_ARGS__)
          |void foo() {
          |  F(a, b, c); // replaced by f(0, a, b, c)
          |  F();        // replaced by f(0)
          |}
          |""".stripMargin)
      pendingUntilFixed {
        // Impossible to test without C++ system headers for __VA_OPT__ and __VA_ARGS__ definitions
        cpg.call.code.l shouldBe List("f(a, b, c)", "f()")
      }
    }

  }
}
