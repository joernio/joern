package io.joern.c2cpg.cpp.features20

import io.joern.c2cpg.parser.FileDefaults
import io.joern.c2cpg.testfixtures.AstC2CpgSuite
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

    "handle concepts" ignore {
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
          |void f(T v);
          |
          |// `T` is a constrained type template parameter.
          |template <typename T>
          |  requires my_concept<T>
          |void f(T v);
          |
          |// `T` is a constrained type template parameter.
          |template <typename T>
          |void f(T v) requires my_concept<T>;
          |
          |// `v` is a constrained deduced parameter.
          |void f(my_concept auto v);
          |
          |// `v` is a constrained non-type template parameter.
          |template <my_concept auto v>
          |void g();
          |
          |// Forms for auto-deduced variables:
          |// `foo` is a constrained auto-deduced value.
          |my_concept auto foo = ...;
          |
          |// Forms for lambdas:
          |// `T` is a constrained type template parameter.
          |auto f = []<my_concept T> (T v) {
          |  // ...
          |};
          |// `T` is a constrained type template parameter.
          |auto f = []<typename T> requires my_concept<T> (T v) {
          |  // ...
          |};
          |// `T` is a constrained type template parameter.
          |auto f = []<typename T> (T v) requires my_concept<T> {
          |  // ...
          |};
          |// `v` is a constrained deduced parameter.
          |auto f = [](my_concept auto v) {
          |  // ...
          |};
          |// `v` is a constrained non-type template parameter.
          |auto g = []<my_concept auto v> () {
          |  // ...
          |};
          |
          |template <typename T>
          |  requires my_concept<T> // `requires` clause.
          |void f(T);
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
          |concept C = requires(T x) {
          |  {*x} -> std::convertible_to<typename T::inner>; // the type of the expression `*x` is convertible to `T::inner`
          |  {x + 1} -> std::same_as<int>; // the expression `x + 1` satisfies `std::same_as<decltype((x + 1))>`
          |  {x * 1} -> std::convertible_to<T>; // the type of the expression `x * 1` is convertible to `T`
          |};
          |
          |template <typename T>
          |concept C = requires(T x) {
          |  requires std::same_as<sizeof(x), size_t>;
          |};
          |""".stripMargin)
      ???
    }

    "handle three-way comparison" ignore {
      val cpg = code("""
          |bool x = 1 <=> 2;
          |""".stripMargin)
      ???
    }

    "handle designated initializers" ignore {
      val cpg = code("""
          |struct A {
          |  int x;
          |  int y;
          |  int z = 123;
          |};
          |
          |A a {.x = 1, .z = 2}; // a.x == 1, a.y == 0, a.z == 2
          |""".stripMargin)
      ???
    }

    "handle template syntax for lambdas" ignore {
      val cpg = code("""
          |auto f = []<typename T>(std::vector<T> v) {
          |  // ...
          |};
          |""".stripMargin)
      ???
    }

    "handle range-based for loop with initializer" ignore {
      val cpg = code("""
          |for (auto v = std::vector{1, 2, 3}; auto& e : v) {
          |  std::cout << e;
          |}
          |// prints "123"
          |""".stripMargin)
      ???
    }

    "handle likely and unlikely attributes" ignore {
      val cpg = code("""
          |switch (n) {
          |case 1:
          |  // ...
          |  break;
          |
          |[[likely]] case 2:  // n == 2 is considered to be arbitrarily more
          |  // ...            // likely than any other value of n
          |  break;
          |}
          |
          |int random = get_random_number_between_x_and_y(0, 3);
          |if (random > 0) [[likely]] {
          |  // body of if statement
          |  // ...
          |}
          |
          |while (unlikely_truthy_condition) [[unlikely]] {
          |  // body of while statement
          |  // ...
          |}
          |""".stripMargin)
      ???
    }

    "handle deprecate implicit capture of this" ignore {
      val cpg = code("""
          |struct int_value {
          |  int n = 0;
          |  auto getter_fn() {
          |    // BAD:
          |    // return [=]() { return n; };
          |
          |    // GOOD:
          |    return [=, *this]() { return n; };
          |  }
          |};
          |""".stripMargin)
      ???
    }

    "handle class types in non-type template parameters" ignore {
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
          |get_foo(); // uses implicit constructor
          |get_foo<foo{123}>();
          |""".stripMargin)
      ???
    }

    "handle constexpr virtual functions" ignore {
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
          |constexpr X4 x4;
          |x4.f(); // == 4
          |""".stripMargin)
      ???
    }

    "handle explicit(bool)" ignore {
      val cpg = code("""
          |struct foo {
          |  // Specify non-integral types (strings, floats, etc.) require explicit construction.
          |  template <typename T>
          |  explicit(!std::is_integral_v<T>) foo(T) {}
          |};
          |
          |foo a = 123;
          |foo c {"123"};
          |""".stripMargin)
      ???
    }

    "handle immediate functions" ignore {
      val cpg = code("""
          |consteval int sqr(int n) {
          |  return n * n;
          |}
          |
          |constexpr int r = sqr(100); // OK
          |int x = 100;
          |int r2 = sqr(x); // ERROR: the value of 'x' is not usable in a constant expression
          |                 // OK if `sqr` were a `constexpr` function
          |""".stripMargin)
      ???
    }

    "handle using enum" ignore {
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
      ???
    }

    "handle lambda capture of parameter pack" ignore {
      val cpg = code("""
          |template <typename... Args>
          |auto f1(Args&&... args){
          |    // BY VALUE:
          |    return [...args = std::forward<Args>(args)] {
          |        // ...
          |    };
          |}
          |
          |template <typename... Args>
          |auto f2(Args&&... args){
          |    // BY REFERENCE:
          |    return [&...args = std::forward<Args>(args)] {
          |        // ...
          |    };
          |}
          |""".stripMargin)
      ???
    }

    "handle char8_t" ignore {
      val cpg = code("""
          |char8_t utf8_str[] = u8"\\u0123";
          |""".stripMargin)
      ???
    }

    "handle constinit" ignore {
      val cpg = code("""
          |const char* g() { return "dynamic initialization"; }
          |constexpr const char* f(bool p) { return p ? "constant initializer" : g(); }
          |
          |constinit const char* c = f(true); // OK
          |constinit const char* d = g(false); // ERROR: `g` is not constexpr, so `d` cannot be evaluated at compile-time.
          |""".stripMargin)
      ???
    }

    "handle __VA_OPT__" ignore {
      val cpg = code("""
          |#define F(...) f(0 __VA_OPT__(,) __VA_ARGS__)
          |F(a, b, c) // replaced by f(0, a, b, c)
          |F()        // replaced by f(0)
          |""".stripMargin)
      ???
    }

  }
}
