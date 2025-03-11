package io.joern.c2cpg.cpp.features17

import io.joern.c2cpg.astcreation.Defines
import io.joern.c2cpg.parser.FileDefaults
import io.joern.c2cpg.testfixtures.AstC2CpgSuite
import io.shiftleft.codepropertygraph.generated.ControlStructureTypes
import io.shiftleft.semanticcpg.language.*
import org.apache.commons.lang3.StringUtils

class Cpp17FeaturesTests extends AstC2CpgSuite(fileSuffix = FileDefaults.CppExt) {

  "C++17 feature support" should {

    "handle template argument deduction for class templates" in {
      val cpg = code("""
          |template <typename T = float>
          |struct MyContainer {
          |  T val;
          |  MyContainer() : val{} {}
          |  MyContainer(T val) : val{val} {}
          |  // ...
          |};
          |MyContainer c1 {1}; // OK MyContainer<int>
          |MyContainer c2; // OK MyContainer<float>
          |""".stripMargin)
      val List(c1, c2) = cpg.local.l
      c1.name shouldBe "c1"
      c1.typeFullName shouldBe "MyContainer<int>"
      c2.name shouldBe "c2"
      c2.typeFullName shouldBe "MyContainer<float>"
      // We are unable to express this template argument deduction in the current schema
      cpg.typeDecl.member.nameExact("val").typeFullName.l shouldBe List("T")
    }

    "handle declaring non-type template parameters with auto" in {
      val cpg = code("""
          |template <typename T, T... Ints>
          |struct integer_sequence {
          |  using value_type = T;
          |  static constexpr std::size_t size() noexcept { return sizeof...(Ints); }
          |};
          |
          |template <auto... seq>
          |struct my_integer_sequence {
          |  // Implementation here ...
          |};
          |
          |// Explicitly pass type `int` as template argument.
          |auto seq = integer_sequence<int, 0, 1, 2>();
          |// Type is deduced to be `int`.
          |auto seq2 = my_integer_sequence<0, 1, 2>();
          |""".stripMargin)
      val List(seq, seq2, ints) = cpg.local.l
      seq.name shouldBe "seq"
      // CDT is unable to deduce the type of the template argument
      seq.typeFullName shouldBe "integer_sequence<int,int0,int1,int2>"
      seq2.name shouldBe "seq2"
      seq2.typeFullName shouldBe "my_integer_sequence<int0,int1,int2>"
      ints.name shouldBe "Ints"
      ints.typeFullName shouldBe "ANY"
    }

    "handle folding expressions (binary)" in {
      val cpg = code("""
          |template <typename... Args>
          |bool logicalAnd(Args... args) {
          |  return (true && ... && args);
          |}
          |bool b = true;
          |bool& b2 = b;
          |logicalAnd(b, b2, true); // == true
          |""".stripMargin)
      val List(argsParam) = cpg.method.nameExact("logicalAnd").parameter.l
      argsParam.name shouldBe "args"
      argsParam.typeFullName shouldBe "Args"
      argsParam.isVariadic shouldBe true
      val List(retExpr) = cpg.method.nameExact("logicalAnd").ast.isReturn.astChildren.isCall.l
      retExpr.name shouldBe "<operator>.fold"
      retExpr.typeFullName shouldBe "bool"
      retExpr.code shouldBe "(true && ... && args)"
      retExpr.argument.code.l shouldBe List("true", "args")
    }

    "handle folding expressions (unary)" in {
      val cpg = code("""
          |template <typename... Args>
          |auto sum(Args... args) {
          |  return (... + args);
          |}
          |sum(1.0, 2.0f, 3); // == 6.0
          |""".stripMargin)
      val List(argsParam) = cpg.method.nameExact("sum").parameter.l
      argsParam.name shouldBe "args"
      argsParam.typeFullName shouldBe "Args"
      argsParam.isVariadic shouldBe true
      val List(retExpr) = cpg.method.nameExact("sum").ast.isReturn.astChildren.isCall.l
      retExpr.name shouldBe "<operator>.fold"
      retExpr.typeFullName shouldBe "Args"
      retExpr.code shouldBe "(... + args)"
      retExpr.argument.code.l shouldBe List("args")
    }

    "handle new rules for auto deduction from braced-init-list" in {
      val cpg = code("""
          |auto x1 = {1, 2, 3}; // x1 is std::initializer_list<int>
          |auto x2 {3}; // x2 is int
          |auto x3 {3.0}; // x3 is double
          |""".stripMargin)
      val List(x1, x2, x3) = cpg.local.l
      x1.name shouldBe "x1"
      x1.typeFullName shouldBe Defines.Any
      x2.name shouldBe "x2"
      x2.typeFullName shouldBe "int"
      x3.name shouldBe "x3"
      x3.typeFullName shouldBe "double"

      pendingUntilFixed {
        // TODO: can not be determined without type information from includes
        x1.typeFullName shouldBe "std::initializer_list<int>"
      }
    }

    "handle constexpr lambda" in {
      val cpg = code("""
          |auto identity = [](int n) constexpr { return n; };
          |constexpr auto add = [](int x, int y) {
          |  auto L = [=] { return x; };
          |  auto R = [=] { return y; };
          |  return [=] { return L() + R(); };
          |};
          |constexpr int addOne(int n) {
          |  return [n] { return n + 1; }();
          |}
          |""".stripMargin)
      cpg.method.nameNot("<global>").fullName.sorted shouldBe List(
        "Test0.cpp:<global>.<lambda>0:int(int)",
        "Test0.cpp:<global>.<lambda>1.<lambda>2:int()",
        "Test0.cpp:<global>.<lambda>1.<lambda>3:int()",
        "Test0.cpp:<global>.<lambda>1.<lambda>4:int()",
        "Test0.cpp:<global>.<lambda>1:std.function(int,int)",
        "Test0.cpp:<global>.addOne.<lambda>5:int()",
        "addOne:int(int)"
      )
    }

    "handle lambda capture `this` by value" in {
      val cpg = code("""
          |struct MyObj {
          |  int value {123};
          |  auto getValueCopy() {
          |    return [*this] { return value; };
          |  }
          |  auto getValueRef() {
          |    return [this] { return value; };
          |  }
          |};
          |MyObj mo;
          |auto valueCopy = mo.getValueCopy();
          |auto valueRef = mo.getValueRef();
          |mo.value = 321;
          |valueCopy(); // 123
          |valueRef(); // 321
          |""".stripMargin)
      // TODO: we can not express these lambda types in the current schema
      // We would need to add a new type for lambdas that capture `this` by value copy/ref.
      cpg.method.nameExact("getValueCopy").methodReturn.typeFullName.l shouldBe List(Defines.Function)
      cpg.method.nameExact("getValueRef").methodReturn.typeFullName.l shouldBe List(Defines.Function)
    }

    "handle inline variables" in {
      val cpg = code("""
          |// Disassembly example using compiler explorer.
          |struct S1 { int x; };
          |inline S1 x1 = S{321}; // mov esi, dword ptr [x1]
          |                       // x1: .long 321
          |
          |S1 x2 = S1{123};      // mov eax, dword ptr [.L_ZZ4mainE2x2]
          |                      // mov dword ptr [rbp - 8], eax
          |                      // .L_ZZ4mainE2x2: .long 123
          |
          |struct S2 {
          |  static inline int count{0}; // declare and initialize count to 0 within the class
          |  S2() : id{count++} {}
          |  ~S2() { count--; }
          |  int id;
          |};
          |""".stripMargin)
      cpg.local.map(l => (l.name, l.typeFullName)).toMap shouldBe Map("x1" -> "S1", "x2" -> "S1", "count" -> "int")
      cpg.typeDecl.member.nameExact("count").typeFullName.l shouldBe List("int")
    }

    "handle nested namespaces" in {
      val cpg = code("""
          |namespace A1 { // old
          |  namespace B1 {
          |    namespace C1 {
          |      int i;
          |    }
          |  }
          |}
          |
          |namespace A2::B2::C2 { // new
          |  int i;
          |}
          |""".stripMargin)
      cpg.namespaceBlock.nameNot("<global>").name.sorted shouldBe List("A1", "A2", "B1", "B2", "C1", "C2")
      cpg.namespaceBlock.nameNot("<global>").fullName.sorted shouldBe List(
        "A1",
        "A1.B1",
        "A1.B1.C1",
        "A2",
        "A2.B2",
        "A2.B2.C2"
      )
    }

    "handle structured bindings" in {
      val cpg = code("""
          |template<class T1, class T2>
          |struct pair {
          |  T1 x;
          |  T2 y;
          |};
          |
          |using Coordinate = pair<int, int>;
          |Coordinate origin() {
          |  return Coordinate{0, 0};
          |}
          |
          |void foo() {
          |  const auto [ x, y ] = origin();
          |  x; // == 0
          |  y; // == 0
          |
          |  std::unordered_map<std::string, int> mapping;
          |  // fill the map ...
          |
          |  // Destructure by reference.
          |  for (const auto& [key, value] : mapping) {
          |    // Do something with key and value
          |  }
          |}
          |""".stripMargin)
      cpg.call.code.l should contain theSameElementsAs List(
        "Coordinate{0, 0}",
        "{0, 0}",
        "<tmp>0 = = origin()",
        "origin()",
        "x = <tmp>0.x",
        "y = <tmp>0.y",
        "<tmp>0.x",
        "<tmp>0.y",
        "<tmp>1 = mapping",
        "key = <tmp>1.key",
        "value = <tmp>1.value",
        "<tmp>1.key",
        "<tmp>1.value"
      )
      cpg.local.map(l => (l.name, l.typeFullName)).toMap shouldBe Map(
        "x"       -> "int",
        "y"       -> "int",
        "<tmp>0"  -> "pair<int,int>",
        "mapping" -> "std.unordered_map<std.string,int>",
        // fails to resolve the type of the structured bindings without C++ header files
        "<tmp>1" -> "ANY",
        "key"    -> "ANY",
        "value"  -> "ANY"
      )
      cpg.controlStructure
        .controlStructureTypeExact(ControlStructureTypes.FOR)
        .astChildren
        .isLocal
        .map(l => (l.name, l.typeFullName))
        .toMap shouldBe Map(
        // fails to resolve the type of the structured bindings without C++ header files
        "<tmp>1" -> "ANY",
        "key"    -> "ANY",
        "value"  -> "ANY"
      )
      pendingUntilFixed {
        cpg.local.map(l => (l.name, l.typeFullName)).toMap shouldBe Map(
          "x"       -> "int",
          "y"       -> "int",
          "<tmp>0"  -> "pair<int,int>",
          "mapping" -> "std.unordered_map<std.string,int>",
          "<tmp>1"  -> "std.unordered_map<std.string,int>",
          "key"     -> "int",
          "value"   -> "int"
        )
        cpg.controlStructure
          .controlStructureTypeExact(ControlStructureTypes.FOR)
          .astChildren
          .astChildren
          .isLocal
          .map(l => (l.name, l.typeFullName))
          .toMap shouldBe Map(
          // fails to resolve the type of the structured bindings without C++ header files
          "<tmp>1" -> "std.unordered_map<std.string,int>",
          "key"    -> "int",
          "value"  -> "int"
        )
      }
    }

    "handle selection statements with initializer" in {
      val cpg = code("""
          |void foo() {
          |  if (std::lock_guard<std::mutex> lk(mx); v.empty()) {
          |    v.push_back(val);
          |  }
          |
          |  switch (Foo gadget(args); auto s = gadget.status()) {
          |    case OK: gadget.zip(); break;
          |    case Bad: throw BadFoo(s.message());
          |  }
          |}
          |""".stripMargin)
      cpg.method
        .nameExact("foo")
        .block
        .astChildren
        .isExpression
        .sortBy(_.argumentIndex)
        .code
        .map(StringUtils.normalizeSpace)
        .l shouldBe List(
        "std::lock_guard<std::mutex> lk(mx)",
        "if (std::lock_guard<std::mutex> lk(mx); v.empty()) { v.push_back(val); }",
        "gadget = Foo(args)",
        "s = gadget.status()",
        "switch (Foo gadget(args); auto s = gadget.status()) { case OK: gadget.zip(); break; case Bad: throw BadFoo(s.message()); }"
      )
    }

    "handle constexpr if" in {
      val cpg = code("""
          |template <typename T>
          |constexpr bool isIntegral() {
          |  if constexpr (std::is_integral<T>::value) {
          |    return true;
          |  } else {
          |    return false;
          |  }
          |}
          |static_assert(isIntegral<int>() == true);
          |static_assert(isIntegral<char>() == true);
          |static_assert(isIntegral<double>() == false);
          |struct S {};
          |static_assert(isIntegral<S>() == false);
          |""".stripMargin)
      cpg.method.nameExact("isIntegral").controlStructure.code.map(StringUtils.normalizeSpace).l shouldBe List(
        "if constexpr (std::is_integral<T>::value) { return true; } else { return false; }",
        "else"
      )
    }

    "handle UTF-8 character literals" in {
      val cpg = code("""
          |void foo() {
          |  char x = u8'x';
          |}
          |""".stripMargin)
      pendingUntilFixed {
        // TODO: not supported  by the CDT parser at the moment
        cpg.assignment.code.l shouldBe List("char x = u8'x'")
        cpg.assignment.argument(2).isLiteral.code.l shouldBe List("u8'x'")
        cpg.local.nameExact("x").typeFullName.l shouldBe List("char")
        cpg.identifier.nameExact("x").typeFullName.l shouldBe List("char")
      }
    }

    "handle direct list initialization of enums" in {
      val cpg = code("""
          |enum byte : unsigned char {};
          |byte b {0};
          |byte d = byte{1};
          |""".stripMargin)
      cpg.local.nameExact("b").typeFullName.l shouldBe List("byte")
      cpg.identifier.nameExact("b").typeFullName.l shouldBe List("byte")
      cpg.local.nameExact("d").typeFullName.l shouldBe List("byte")
      cpg.identifier.nameExact("d").typeFullName.l shouldBe List("byte")
    }

    "handle fallthrough, nodiscard, maybe_unused attributes" in {
      val cpg = code("""
          |void foo() {
          |  switch (n) {
          |    case 1:
          |      // ...
          |      [[fallthrough]];
          |    case 2:
          |      // ...
          |      break;
          |    case 3:
          |      // ...
          |      [[fallthrough]];
          |    default:
          |      // ...
          |  }
          |}
          |
          |[[nodiscard]] bool do_something() {
          |  return is_success; // true for success, false for failure
          |}
          |struct [[nodiscard]] error_info {
          |  // ...
          |};
          |
          |void my_callback(std::string msg, [[maybe_unused]] bool error) {
          |  // Don't care if `msg` is an error message, just log it.
          |  log(msg);
          |}
          |""".stripMargin)
      cpg.method
        .nameExact("foo")
        .controlStructure
        .controlStructureTypeExact(ControlStructureTypes.SWITCH)
        .astChildren
        .isBlock
        ._jumpTargetViaAstOut
        .code
        .l shouldBe List("case 1:", "case 2:", "case 3:", "default:")
      cpg.method.nameExact("do_something").size shouldBe 1
      cpg.typeDecl.nameExact("error_info").size shouldBe 1
      cpg.method.nameExact("my_callback").parameter.name.l shouldBe List("msg", "error")
    }

    "handle _has_include" in {
      val cpg = code("""
          |#ifdef __has_include
          |#  if __has_include(<optional>)
          |#    include <optional>
          |#    define have_optional 1
          |#  elif __has_include(<experimental/optional>)
          |#    include <experimental/optional>
          |#    define have_optional 1
          |#    define experimental_optional
          |#  else
          |#    define have_optional 0
          |#  endif
          |#endif
          |
          |#ifdef __has_include
          |#  if __has_include(<x.h>)
          |#    include <x.h>
          |#  elif __has_include(<y.h>)
          |#    include <y.h>
          |#  else
          |#    error No suitable headers found.
          |# endif
          |#endif
          |""".stripMargin)
        .moreCode(
          """
          |int x = 1;
          |""".stripMargin,
          "x.h"
        )
        .moreCode(
          """
          |int y = 1;
          |""".stripMargin,
          "y.h"
        )
      cpg.imports.code.l shouldBe List(
        "#    include <optional>",
        "#    include <experimental/optional>",
        "#    include <x.h>",
        "#    include <y.h>"
      )
      cpg.local.name.l shouldBe List("x", "y")
    }

    "handle class template argument deduction" in {
      val cpg = code("""
          |template <typename T>
          |struct container {
          |  container(T t) {}
          |  template <typename Iter>
          |  container(Iter beg, Iter end);
          |};
          |
          |template <typename Iter>
          |container(Iter b, Iter e) -> container<typename std::iterator_traits<Iter>::value_type>;
          |
          |void foo() {
          |  std::vector v{ 1, 2, 3 }; // deduces std::vector<int>
          |  std::mutex mtx;
          |  auto lck = std::lock_guard{ mtx }; // deduces to std::lock_guard<std::mutex>
          |  auto p = new std::pair{ 1.0, 2.0 }; // deduces to std::pair<double, double>*
          |
          |  container a{ 7 }; // OK: deduces container<int>
          |  std::vector<double> v{ 1.0, 2.0, 3.0 };
          |  auto b = container{ v.begin(), v.end() }; // OK: deduces container<double>
          |}
          |""".stripMargin)
      cpg.local.nameExact("a").typeFullName.l shouldBe List("container<int>")
      cpg.local.nameExact("v").typeFullName.l shouldBe List(
        "std.vector", // generic types are not deduced
        "std.vector<double>"
      )
      pendingUntilFixed {
        // CDT deduces them to ProblemType as there are no includes for std::
        cpg.local.nameExact("p").typeFullName.l shouldBe List("std.pair<double,double>*")
        cpg.local.nameExact("lck").typeFullName.l shouldBe List("std.lock_guard<std.mutex>")
        cpg.local.nameExact("b").typeFullName.l shouldBe List("container<double>")
      }
    }

  }
}
