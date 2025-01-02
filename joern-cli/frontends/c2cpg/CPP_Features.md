# Support For New Language Features

* adapted from https://github.com/AnthonyCalandra/modern-cpp-features/tree/master)
* Table legend:
  * `[ ]` not supported at all / can not even be parsed
  * `[-]` can be parsed but is not represented in the CPG
  * `[x]` full support including the CPG representation

## C++17 New Language Features Overview


| Feature                                                                                                                   |       Supported       |
|---------------------------------------------------------------------------------------------------------------------------|-----------------------|
| [template argument deduction for class templates](#template-argument-deduction-for-class-templates)                       |          [ ]          |
| [declaring non-type template parameters with auto](#declaring-non-type-template-parameters-with-auto)                     |          [ ]          |
| [folding expressions](#folding-expressions)                                                                               |          [ ]          |
| [new rules for auto deduction from braced-init-list](#new-rules-for-auto-deduction-from-braced-init-list)                 |          [ ]          |
| [constexpr lambda](#constexpr-lambda)                                                                                     |          [ ]          |
| [lambda capture this by value](#lambda-capture-this-by-value)                                                             |          [ ]          |
| [inline variables](#inline-variables)                                                                                     |          [ ]          |
| [nested namespaces](#nested-namespaces)                                                                                   |          [ ]          |
| [structured bindings](#structured-bindings)                                                                               |          [ ]          |
| [selection statements with initializer](#selection-statements-with-initializer)                                           |          [ ]          |
| [constexpr if](#constexpr-if)                                                                                             |          [ ]          |
| [utf-8 character literals](#utf-8-character-literals)                                                                     |          [ ]          |
| [direct-list-initialization of enums](#direct-list-initialization-of-enums)                                               |          [ ]          |
| [\[\[fallthrough\]\], \[\[nodiscard\]\], \[\[maybe_unused\]\] attributes](#fallthrough-nodiscard-maybe_unused-attributes) |          [ ]          |
| [\_\_has\_include](#\_\_has\_include)                                                                                     |          [ ]          |
| [class template argument deduction](#class-template-argument-deduction)                                                   |          [ ]          |


### Template argument deduction for class templates
Automatic template argument deduction much like how it's done for functions, but now including class constructors.
```c++
template <typename T = float>
struct MyContainer {
  T val;
  MyContainer() : val{} {}
  MyContainer(T val) : val{val} {}
  // ...
};
MyContainer c1 {1}; // OK MyContainer<int>
MyContainer c2; // OK MyContainer<float>
```

### Declaring non-type template parameters with auto
Following the deduction rules of `auto`, while respecting the non-type template parameter list of allowable types[\*], template arguments can be deduced from the types of its arguments:
```c++
template <auto... seq>
struct my_integer_sequence {
  // Implementation here ...
};

// Explicitly pass type `int` as template argument.
auto seq = std::integer_sequence<int, 0, 1, 2>();
// Type is deduced to be `int`.
auto seq2 = my_integer_sequence<0, 1, 2>();
```
\* - For example, you cannot use a `double` as a template parameter type, which also makes this an invalid deduction using `auto`.

### Folding expressions
A fold expression performs a fold of a template parameter pack over a binary operator.
* An expression of the form `(... op e)` or `(e op ...)`, where `op` is a fold-operator and `e` is an unexpanded parameter pack, are called _unary folds_.
* An expression of the form `(e1 op ... op e2)`, where `op` are fold-operators, is called a _binary fold_. Either `e1` or `e2` is an unexpanded parameter pack, but not both.
```c++
template <typename... Args>
bool logicalAnd(Args... args) {
    // Binary folding.
    return (true && ... && args);
}
bool b = true;
bool& b2 = b;
logicalAnd(b, b2, true); // == true
```
```c++
template <typename... Args>
auto sum(Args... args) {
    // Unary folding.
    return (... + args);
}
sum(1.0, 2.0f, 3); // == 6.0
```

### New rules for auto deduction from braced-init-list
Changes to `auto` deduction when used with the uniform initialization syntax. Previously, `auto x {3};` deduces a `std::initializer_list<int>`, which now deduces to `int`.
```c++
auto x1 {1, 2, 3}; // error: not a single element
auto x2 = {1, 2, 3}; // x2 is std::initializer_list<int>
auto x3 {3}; // x3 is int
auto x4 {3.0}; // x4 is double
```

### constexpr lambda
Compile-time lambdas using `constexpr`.
```c++
auto identity = [](int n) constexpr { return n; };
static_assert(identity(123) == 123);
```
```c++
constexpr auto add = [](int x, int y) {
  auto L = [=] { return x; };
  auto R = [=] { return y; };
  return [=] { return L() + R(); };
};

static_assert(add(1, 2)() == 3);
```
```c++
constexpr int addOne(int n) {
  return [n] { return n + 1; }();
}

static_assert(addOne(1) == 2);
```

### Lambda capture `this` by value
Capturing `this` in a lambda's environment was previously reference-only. An example of where this is problematic is asynchronous code using callbacks that require an object to be available, potentially past its lifetime. `*this` (C++17) will now make a copy of the current object, while `this` (C++11) continues to capture by reference.
```c++
struct MyObj {
  int value {123};
  auto getValueCopy() {
    return [*this] { return value; };
  }
  auto getValueRef() {
    return [this] { return value; };
  }
};
MyObj mo;
auto valueCopy = mo.getValueCopy();
auto valueRef = mo.getValueRef();
mo.value = 321;
valueCopy(); // 123
valueRef(); // 321
```

### Inline variables
The inline specifier can be applied to variables as well as to functions. A variable declared inline has the same semantics as a function declared inline.
```c++
// Disassembly example using compiler explorer.
struct S { int x; };
inline S x1 = S{321}; // mov esi, dword ptr [x1]
                      // x1: .long 321

S x2 = S{123};        // mov eax, dword ptr [.L_ZZ4mainE2x2]
                      // mov dword ptr [rbp - 8], eax
                      // .L_ZZ4mainE2x2: .long 123
```

It can also be used to declare and define a static member variable, such that it does not need to be initialized in the source file.
```c++
struct S {
  S() : id{count++} {}
  ~S() { count--; }
  int id;
  static inline int count{0}; // declare and initialize count to 0 within the class
};
```

### Nested namespaces
Using the namespace resolution operator to create nested namespace definitions.
```c++
namespace A {
  namespace B {
    namespace C {
      int i;
    }
  }
}
```

The code above can be written like this:
```c++
namespace A::B::C {
  int i;
}
```

### Structured bindings
A proposal for de-structuring initialization, that would allow writing `auto [ x, y, z ] = expr;` where the type of `expr` was a tuple-like object, whose elements would be bound to the variables `x`, `y`, and `z` (which this construct declares). _Tuple-like objects_ include [`std::tuple`](README.md#tuples), `std::pair`, [`std::array`](README.md#stdarray), and aggregate structures.
```c++
using Coordinate = std::pair<int, int>;
Coordinate origin() {
  return Coordinate{0, 0};
}

const auto [ x, y ] = origin();
x; // == 0
y; // == 0
```
```c++
std::unordered_map<std::string, int> mapping {
  {"a", 1},
  {"b", 2},
  {"c", 3}
};

// Destructure by reference.
for (const auto& [key, value] : mapping) {
  // Do something with key and value
}
```

### Selection statements with initializer
New versions of the `if` and `switch` statements which simplify common code patterns and help users keep scopes tight.
```c++
{
  std::lock_guard<std::mutex> lk(mx);
  if (v.empty()) v.push_back(val);
}
// vs.
if (std::lock_guard<std::mutex> lk(mx); v.empty()) {
  v.push_back(val);
}
```
```c++
Foo gadget(args);
switch (auto s = gadget.status()) {
  case OK: gadget.zip(); break;
  case Bad: throw BadFoo(s.message());
}
// vs.
switch (Foo gadget(args); auto s = gadget.status()) {
  case OK: gadget.zip(); break;
  case Bad: throw BadFoo(s.message());
}
```

### constexpr if
Write code that is instantiated depending on a compile-time condition.
```c++
template <typename T>
constexpr bool isIntegral() {
  if constexpr (std::is_integral<T>::value) {
    return true;
  } else {
    return false;
  }
}
static_assert(isIntegral<int>() == true);
static_assert(isIntegral<char>() == true);
static_assert(isIntegral<double>() == false);
struct S {};
static_assert(isIntegral<S>() == false);
```

### UTF-8 character literals
A character literal that begins with `u8` is a character literal of type `char`. The value of a UTF-8 character literal is equal to its ISO 10646 code point value.
```c++
char x = u8'x';
```

### Direct list initialization of enums
Enums can now be initialized using braced syntax.
```c++
enum byte : unsigned char {};
byte b {0}; // OK
byte c {-1}; // ERROR
byte d = byte{1}; // OK
byte e = byte{256}; // ERROR
```

### \[\[fallthrough\]\], \[\[nodiscard\]\], \[\[maybe_unused\]\] attributes
C++17 introduces three new attributes: `[[fallthrough]]`, `[[nodiscard]]` and `[[maybe_unused]]`.
* `[[fallthrough]]` indicates to the compiler that falling through in a switch statement is intended behavior. This attribute may only be used in a switch statement, and must be placed before the next case/default label.
```c++
switch (n) {
  case 1: 
    // ...
    [[fallthrough]];
  case 2:
    // ...
    break;
  case 3:
    // ...
    [[fallthrough]];
  default:
    // ...
}
```

* `[[nodiscard]]` issues a warning when either a function or class has this attribute and its return value is discarded.
```c++
[[nodiscard]] bool do_something() {
  return is_success; // true for success, false for failure
}

do_something(); // warning: ignoring return value of 'bool do_something()',
                // declared with attribute 'nodiscard'
```
```c++
// Only issues a warning when `error_info` is returned by value.
struct [[nodiscard]] error_info {
  // ...
};

error_info do_something() {
  error_info ei;
  // ...
  return ei;
}

do_something(); // warning: ignoring returned value of type 'error_info',
                // declared with attribute 'nodiscard'
```

* `[[maybe_unused]]` indicates to the compiler that a variable or parameter might be unused and is intended.
```c++
void my_callback(std::string msg, [[maybe_unused]] bool error) {
  // Don't care if `msg` is an error message, just log it.
  log(msg);
}
```

### \_\_has\_include

`__has_include (operand)` operator may be used in `#if` and `#elif` expressions to check whether a header or source file (`operand`) is available for inclusion or not.

One use case of this would be using two libraries that work the same way, using the backup/experimental one if the preferred one is not found on the system.

```c++
#ifdef __has_include
#  if __has_include(<optional>)
#    include <optional>
#    define have_optional 1
#  elif __has_include(<experimental/optional>)
#    include <experimental/optional>
#    define have_optional 1
#    define experimental_optional
#  else
#    define have_optional 0
#  endif
#endif
```

It can also be used to include headers existing under different names or locations on various platforms, without knowing which platform the program is running on, OpenGL headers are a good example for this which are located in `OpenGL\` directory on macOS and `GL\` on other platforms.

```c++
#ifdef __has_include
#  if __has_include(<OpenGL/gl.h>)
#    include <OpenGL/gl.h>
#    include <OpenGL/glu.h>
#  elif __has_include(<GL/gl.h>)
#    include <GL/gl.h>
#    include <GL/glu.h>
#  else
#    error No suitable OpenGL headers found.
# endif
#endif
```

### Class template argument deduction
*Class template argument deduction* (CTAD) allows the compiler to deduce template arguments from constructor arguments.
```c++
std::vector v{ 1, 2, 3 }; // deduces std::vector<int>

std::mutex mtx;
auto lck = std::lock_guard{ mtx }; // deduces to std::lock_guard<std::mutex>

auto p = new std::pair{ 1.0, 2.0 }; // deduces to std::pair<double, double>*
```

For user-defined types, *deduction guides* can be used to guide the compiler how to deduce template arguments if applicable:
```c++
template <typename T>
struct container {
  container(T t) {}

  template <typename Iter>
  container(Iter beg, Iter end);
};

// deduction guide
template <typename Iter>
container(Iter b, Iter e) -> container<typename std::iterator_traits<Iter>::value_type>;

container a{ 7 }; // OK: deduces container<int>

std::vector<double> v{ 1.0, 2.0, 3.0 };
auto b = container{ v.begin(), v.end() }; // OK: deduces container<double>

container c{ 5, 6 }; // ERROR: std::iterator_traits<int>::value_type is not a type
```


## C++20 New Language Features Overview

| Feature                                                                                     |       Supported        |
|---------------------------------------------------------------------------------------------|------------------------|
| [coroutines](#coroutines)                                                                   |          [ ]           |
| [concepts](#concepts)                                                                       |          [ ]           |
| [three-way comparison](#three-way-comparison)                                               |          [ ]           |
| [designated initializers](#designated-initializers)                                         |          [ ]           |
| [template syntax for lambdas](#template-syntax-for-lambdas)                                 |          [ ]           |
| [range-based for loop with initializer](#range-based-for-loop-with-initializer)             |          [ ]           |
| [\[\[likely\]\] and \[\[unlikely\]\] attributes](#likely-and-unlikely-attributes)           |          [ ]           |
| [deprecate implicit capture of this](#deprecate-implicit-capture-of-this)                   |          [ ]           |
| [class types in non-type template parameters](#class-types-in-non-type-template-parameters) |          [ ]           |
| [constexpr virtual functions](#constexpr-virtual-functions)                                 |          [ ]           |
| [explicit(bool)](#explicitbool)                                                             |          [ ]           |
| [immediate functions](#immediate-functions)                                                 |          [ ]           |
| [using enum](#using-enum)                                                                   |          [ ]           |
| [lambda capture of parameter pack](#lambda-capture-of-parameter-pack)                       |          [ ]           |
| [char8_t](#char8_t)                                                                         |          [ ]           |
| [constinit](#constinit)                                                                     |          [ ]           |
| [\_\_VA\_OPT\_\_](#__VA_OPT__)                                                              |          [ ]           |


### Coroutines

_Coroutines_ are special functions that can have their execution suspended and resumed. To define a coroutine, the `co_return`, `co_await`, or `co_yield` keywords must be present in the function's body. C++20's coroutines are stackless; unless optimized out by the compiler, their state is allocated on the heap.

An example of a coroutine is a _generator_ function, which yields (i.e. generates) a value at each invocation:
```c++
generator<int> range(int start, int end) {
  while (start < end) {
    co_yield start;
    start++;
  }

  // Implicit co_return at the end of this function:
  // co_return;
}

for (int n : range(0, 10)) {
  std::cout << n << std::endl;
}
```
The above `range` generator function generates values starting at `start` until `end` (exclusive), with each iteration step yielding the current value stored in `start`. The generator maintains its state across each invocation of `range` (in this case, the invocation is for each iteration in the for loop). `co_yield` takes the given expression, yields (i.e. returns) its value, and suspends the coroutine at that point. Upon resuming, execution continues after the `co_yield`.

Another example of a coroutine is a _task_, which is an asynchronous computation that is executed when the task is awaited:
```c++
task<void> echo(socket s) {
  for (;;) {
    auto data = co_await s.async_read();
    co_await async_write(s, data);
  }

  // Implicit co_return at the end of this function:
  // co_return;
}
```
In this example, the `co_await` keyword is introduced. This keyword takes an expression and suspends execution if the thing you're awaiting on (in this case, the read or write) is not ready, otherwise you continue execution. (Note that under the hood, `co_yield` uses `co_await`.)

Using a task to lazily evaluate a value:
```c++
task<int> calculate_meaning_of_life() {
  co_return 42;
}

auto meaning_of_life = calculate_meaning_of_life();
// ...
co_await meaning_of_life; // == 42
```

### Concepts
_Concepts_ are named compile-time predicates which constrain types. They take the following form:
```
template < template-parameter-list >
concept concept-name = constraint-expression;
```
where `constraint-expression` evaluates to a constexpr Boolean. _Constraints_ should model semantic requirements, such as whether a type is a numeric or hashable. A compiler error results if a given type does not satisfy the concept it's bound by (i.e. `constraint-expression` returns `false`). Because constraints are evaluated at compile-time, they can provide more meaningful error messages and runtime safety.
```c++
// `T` is not limited by any constraints.
template <typename T>
concept always_satisfied = true;
// Limit `T` to integrals.
template <typename T>
concept integral = std::is_integral_v<T>;
// Limit `T` to both the `integral` constraint and signedness.
template <typename T>
concept signed_integral = integral<T> && std::is_signed_v<T>;
// Limit `T` to both the `integral` constraint and the negation of the `signed_integral` constraint.
template <typename T>
concept unsigned_integral = integral<T> && !signed_integral<T>;
```
There are a variety of syntactic forms for enforcing concepts:
```c++
// Forms for function parameters:
// `T` is a constrained type template parameter.
template <my_concept T>
void f(T v);

// `T` is a constrained type template parameter.
template <typename T>
  requires my_concept<T>
void f(T v);

// `T` is a constrained type template parameter.
template <typename T>
void f(T v) requires my_concept<T>;

// `v` is a constrained deduced parameter.
void f(my_concept auto v);

// `v` is a constrained non-type template parameter.
template <my_concept auto v>
void g();

// Forms for auto-deduced variables:
// `foo` is a constrained auto-deduced value.
my_concept auto foo = ...;

// Forms for lambdas:
// `T` is a constrained type template parameter.
auto f = []<my_concept T> (T v) {
  // ...
};
// `T` is a constrained type template parameter.
auto f = []<typename T> requires my_concept<T> (T v) {
  // ...
};
// `T` is a constrained type template parameter.
auto f = []<typename T> (T v) requires my_concept<T> {
  // ...
};
// `v` is a constrained deduced parameter.
auto f = [](my_concept auto v) {
  // ...
};
// `v` is a constrained non-type template parameter.
auto g = []<my_concept auto v> () {
  // ...
};
```
The `requires` keyword is used either to start a `requires` clause or a `requires` expression:
```c++
template <typename T>
  requires my_concept<T> // `requires` clause.
void f(T);

template <typename T>
concept callable = requires (T f) { f(); }; // `requires` expression.

template <typename T>
  requires requires (T x) { x + x; } // `requires` clause and expression on same line.
T add(T a, T b) {
  return a + b;
}
```
Note that the parameter list in a `requires` expression is optional. Each requirement in a `requires` expression are one of the following:

* **Simple requirements** - asserts that the given expression is valid.

```c++
template <typename T>
concept callable = requires (T f) { f(); };
```
* **Type requirements** - denoted by the `typename` keyword followed by a type name, asserts that the given type name is valid.

```c++
struct foo {
  int foo;
};

struct bar {
  using value = int;
  value data;
};

struct baz {
  using value = int;
  value data;
};

// Using SFINAE, enable if `T` is a `baz`.
template <typename T, typename = std::enable_if_t<std::is_same_v<T, baz>>>
struct S {};

template <typename T>
using Ref = T&;

template <typename T>
concept C = requires {
                     // Requirements on type `T`:
  typename T::value; // A) has an inner member named `value`
  typename S<T>;     // B) must have a valid class template specialization for `S`
  typename Ref<T>;   // C) must be a valid alias template substitution
};

template <C T>
void g(T a);

g(foo{}); // ERROR: Fails requirement A.
g(bar{}); // ERROR: Fails requirement B.
g(baz{}); // PASS.
```
* **Compound requirements** - an expression in braces followed by a trailing return type or type constraint.

```c++
template <typename T>
concept C = requires(T x) {
  {*x} -> std::convertible_to<typename T::inner>; // the type of the expression `*x` is convertible to `T::inner`
  {x + 1} -> std::same_as<int>; // the expression `x + 1` satisfies `std::same_as<decltype((x + 1))>`
  {x * 1} -> std::convertible_to<T>; // the type of the expression `x * 1` is convertible to `T`
};
```
* **Nested requirements** - denoted by the `requires` keyword, specify additional constraints (such as those on local parameter arguments).

```c++
template <typename T>
concept C = requires(T x) {
  requires std::same_as<sizeof(x), size_t>;
};
```

### Three-way comparison
C++20 introduces the spaceship operator (`<=>`) as a new way to write comparison functions that reduce boilerplate and help developers define clearer comparison semantics. Defining a three-way comparison operator will autogenerate the other comparison operator functions (i.e. `==`, `!=`, `<`, etc.).

Three orderings are introduced:
* `std::strong_ordering`: The strong ordering distinguishes between items being equal (identical and interchangeable). Provides `less`, `greater`, `equivalent`, and `equal` ordering. Examples of comparisons: searching for a specific value in a list, values of integers, case-sensitive strings.
* `std::weak_ordering`: The weak ordering distinguishes between items being equivalent (not identical, but can be interchangeable for the purposes of comparison). Provides `less`, `greater`, and `equivalent` ordering. Examples of comparisons: case-insensitive strings, sorting, comparing some but not all visible members of a class.
* `std::partial_ordering`: The partial ordering follows the same principle of weak ordering but includes the case when an ordering isn't possible. Provides `less`, `greater`, `equivalent`, and `unordered` ordering. Examples of comparisons: floating-point values (e.g. `NaN`).

A defaulted three-way comparison operator does a member-wise comparison:
```c++
struct foo {
  int a;
  bool b;
  char c;

  // Compare `a` first, then `b`, then `c` ...
  auto operator<=>(const foo&) const = default;
};

foo f1{0, false, 'a'}, f2{0, true, 'b'};
f1 < f2; // == true
f1 == f2; // == false
f1 >= f2; // == false
```

You can also define your own comparisons:
```c++
struct foo {
  int x;
  bool b;
  char c;
  std::strong_ordering operator<=>(const foo& other) const {
      return x <=> other.x;
  }
};

foo f1{0, false, 'a'}, f2{0, true, 'b'};
f1 < f2; // == false
f1 == f2; // == true
f1 >= f2; // == true
```

### Designated initializers
C-style designated initializer syntax. Any member fields that are not explicitly listed in the designated initializer list are default-initialized.
```c++
struct A {
  int x;
  int y;
  int z = 123;
};

A a {.x = 1, .z = 2}; // a.x == 1, a.y == 0, a.z == 2
```

### Template syntax for lambdas
Use familiar template syntax in lambda expressions.
```c++
auto f = []<typename T>(std::vector<T> v) {
  // ...
};
```

### Range-based for loop with initializer
This feature simplifies common code patterns, helps keep scopes tight, and offers an elegant solution to a common lifetime problem.
```c++
for (auto v = std::vector{1, 2, 3}; auto& e : v) {
  std::cout << e;
}
// prints "123"
```

### \[\[likely\]\] and \[\[unlikely\]\] attributes
Provides a hint to the optimizer that the labelled statement has a high probability of being executed.
```c++
switch (n) {
case 1:
  // ...
  break;

[[likely]] case 2:  // n == 2 is considered to be arbitrarily more
  // ...            // likely than any other value of n
  break;
}
```

If one of the likely/unlikely attributes appears after the right parenthesis of an if-statement,
it indicates that the branch is likely/unlikely to have its substatement (body) executed.
```c++
int random = get_random_number_between_x_and_y(0, 3);
if (random > 0) [[likely]] {
  // body of if statement
  // ...
}
```

It can also be applied to the substatement (body) of an iteration statement.
```c++
while (unlikely_truthy_condition) [[unlikely]] {
  // body of while statement
  // ...
}
```

### Deprecate implicit capture of this
Implicitly capturing `this` in a lambda capture using `[=]` is now deprecated; prefer capturing explicitly using `[=, this]` or `[=, *this]`.
```c++
struct int_value {
  int n = 0;
  auto getter_fn() {
    // BAD:
    // return [=]() { return n; };

    // GOOD:
    return [=, *this]() { return n; };
  }
};
```

### Class types in non-type template parameters
Classes can now be used in non-type template parameters. Objects passed in as template arguments have the type `const T`, where `T` is the type of the object, and has static storage duration.
```c++
struct foo {
  foo() = default;
  constexpr foo(int) {}
};

template <foo f = {}>
auto get_foo() {
  return f;
}

get_foo(); // uses implicit constructor
get_foo<foo{123}>();
```

### constexpr virtual functions
Virtual functions can now be `constexpr` and evaluated at compile-time. `constexpr` virtual functions can override non-`constexpr` virtual functions and vice-versa.
```c++
struct X1 {
  virtual int f() const = 0;
};

struct X2: public X1 {
  constexpr virtual int f() const { return 2; }
};

struct X3: public X2 {
  virtual int f() const { return 3; }
};

struct X4: public X3 {
  constexpr virtual int f() const { return 4; }
};

constexpr X4 x4;
x4.f(); // == 4
```

### explicit(bool)
Conditionally select at compile-time whether a constructor is made explicit or not. `explicit(true)` is the same as specifying `explicit`.
```c++
struct foo {
  // Specify non-integral types (strings, floats, etc.) require explicit construction.
  template <typename T>
  explicit(!std::is_integral_v<T>) foo(T) {}
};

foo a = 123; // OK
foo b = "123"; // ERROR: explicit constructor is not a candidate (explicit specifier evaluates to true)
foo c {"123"}; // OK
```

### Immediate functions
Similar to `constexpr` functions, but functions with a `consteval` specifier must produce a constant. These are called `immediate functions`.
```c++
consteval int sqr(int n) {
  return n * n;
}

constexpr int r = sqr(100); // OK
int x = 100;
int r2 = sqr(x); // ERROR: the value of 'x' is not usable in a constant expression
                 // OK if `sqr` were a `constexpr` function
```

### using enum
Bring an enum's members into scope to improve readability. Before:
```c++
enum class rgba_color_channel { red, green, blue, alpha };

std::string_view to_string(rgba_color_channel channel) {
  switch (channel) {
    case rgba_color_channel::red:   return "red";
    case rgba_color_channel::green: return "green";
    case rgba_color_channel::blue:  return "blue";
    case rgba_color_channel::alpha: return "alpha";
  }
}
```
After:
```c++
enum class rgba_color_channel { red, green, blue, alpha };

std::string_view to_string(rgba_color_channel my_channel) {
  switch (my_channel) {
    using enum rgba_color_channel;
    case red:   return "red";
    case green: return "green";
    case blue:  return "blue";
    case alpha: return "alpha";
  }
}
```

### Lambda capture of parameter pack
Capture parameter packs by value:
```c++
template <typename... Args>
auto f(Args&&... args){
    // BY VALUE:
    return [...args = std::forward<Args>(args)] {
        // ...
    };
}
```
Capture parameter packs by reference:
```c++
template <typename... Args>
auto f(Args&&... args){
    // BY REFERENCE:
    return [&...args = std::forward<Args>(args)] {
        // ...
    };
}
```

### char8_t
Provides a standard type for representing UTF-8 strings.
```c++
char8_t utf8_str[] = u8"\u0123";
```

### constinit
The `constinit` specifier requires that a variable must be initialized at compile-time.
```c++
const char* g() { return "dynamic initialization"; }
constexpr const char* f(bool p) { return p ? "constant initializer" : g(); }

constinit const char* c = f(true); // OK
constinit const char* d = g(false); // ERROR: `g` is not constexpr, so `d` cannot be evaluated at compile-time.
```

### \_\_VA\_OPT\_\_
Helps support variadic macros by evaluating to the given argument if the variadic macro is non-empty.
```c++
#define F(...) f(0 __VA_OPT__(,) __VA_ARGS__)
F(a, b, c) // replaced by f(0, a, b, c)
F()        // replaced by f(0)
```
