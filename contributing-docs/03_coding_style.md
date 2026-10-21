Joern coding style and best practices
=====================================

Most of the style rules are enforced automatically, by scalafmt, by the compiler and by our custom scalafix rules.
CI checks all of them on every pull request. This page lists the rules, and the ones that are not automated yet.

Run before you submit
---------------------

```bash
sbt "scalafmt ; Test/scalafmt ; integrationTests/scalafmt ; integrationTests/Test/scalafmt"
sbt "scalafix --diff-base origin/master RestrictedImports SingleLetterIdentifiers UnorderedIteration"
```

`scalafix --diff-base origin/master` only checks the lines you changed. The Bazel equivalent, which checks all
changed files, is `bazel run scalafix` (see [bazel.md](../bazel.md)).

Formatting
----------

- scalafmt, configured in [.scalafmt.conf](../.scalafmt.conf): `preset = IntelliJ`, `maxColumn = 120`,
  `align.preset = true`. Do not fight the formatter. Do not reformat unrelated code in a PR.

Scala dialect
-------------

The compiler flags are set in `compilerSettings` in [build.sbt](../build.sbt):

- **Braces, not indentation.** The build uses `-no-indent -old-syntax`, so write `if (x) { ... } else { ... }`,
  `xs.map { x => ... }` and `class Foo { ... }`. Do not use fewer-braces syntax or `end` markers.
  Note that this is the opposite of the sbt project's style.
- `-Werror`: warnings fail the build. Do not silence a warning without a reason, and prefer fixing the cause.
  Deprecations are reported (`-deprecation`).
- Code is compiled with `--release 17`. Use no APIs newer than Java 17.
- Use `*` for wildcard imports in new code (`import io.shiftleft.semanticcpg.language.*`).

Rules enforced by scalafix
--------------------------

The rules live in [linter-rules](../linter-rules) and are applied to all modules. See also
[linter-rules/src/main/scala/fix](../linter-rules/src/main/scala/fix).

### RestrictedImports

- Do not use `better.files`. Use `io.shiftleft.semanticcpg.utils.FileUtil`, `os-lib` or `java.nio.file`.
- Do not use `scala.sys.process`. Use `io.shiftleft.semanticcpg.utils.ExternalCommand`.

### SingleLetterIdentifiers

- Do not use single-letter names for parameters, lambda parameters and local variables.
  Only `i`, `j`, `k` and `_` are allowed.

```scala
// BAD
cpg.method.map(m => m.name)

// GOOD
cpg.method.map(method => method.name)
```

### UnorderedIteration

CPG creation must be deterministic. Iterating over a `HashMap`, `HashSet`, `TrieMap` or a Java hash map yields a
different order across runs, and that can change node ids and edge order in the output.

- Order-sensitive operations (`foreach`, `map`, `toList`, `find`, `head`, `sum`, `minBy`, `for` generators, ...)
  on unordered collections are flagged.
- Order-independent operations (`contains`, `size`, `get`, `exists`, `forall`, `count`) are fine.
- Fix it by sorting first, or by using an ordered collection (`LinkedHashMap`, `TreeMap`, `List`).
- If the order really does not matter, suppress it on that line, and say why:

```scala
unorderedMap.foreach { case (key, value) => register(key, value) } // scalafix:ok UnorderedIteration
```

Changing a rule or adding a new one: see [linter-rules/src/test](../linter-rules/src/test) and the fixtures in
`linter-rules/input`. Test the rules with `sbt linterRules/test`.

General style
-------------

### Naming

- Use short names for small scopes, longer and descriptive names for larger scopes.
- Packages follow `io.joern.{module}`, with the frontends as `io.joern.{frontend}`
  (for example `io.joern.c2cpg.astcreation`). `semanticcpg` uses `io.shiftleft.semanticcpg`.

### Functional programming

- Prefer pure, succinct functions and immutable data.
- Use `Option` instead of `null`.
- Prefer functional constructs such as `.map` and `.foldLeft` instead of `while` loops, unless a loop is much
  clearer or is a measured hot path.
- Avoid `return` statements.
- Prefer `scala.util.Using` over `try`/`finally` for resources.

### Files, processes and paths

- Code must run on Linux, macOS and Windows. Do not assume `/` as separator in strings, and do not rely on
  shell features. Use `java.nio.file.Path` and `FileUtil`.
- Normalize paths and file names stored in the CPG so that output does not depend on the platform.
- Use `FileUtil.usingTemporaryDirectory` for temporary directories, so that they get cleaned up.
- Start external programs with `ExternalCommand`, not with `Runtime.exec` or `ProcessBuilder` directly.

### Logging

- Use SLF4J (`LoggerFactory.getLogger(classOf[Foo])`), not `println`.

### Imports

- Put all imports at the top of the file.
- Use `scala.jdk.CollectionConverters.*` for Java collection conversions (`JavaConverters` is deprecated).

### Comments

- Use ScalaDoc to document public API, and document the intent and background rather than transcribing code to
  English.
- Avoid inline comments that repeat the code.

Modular design
--------------

- Put code in the lowest module where it makes sense, but not lower. Language-independent helpers belong in
  `x2cpg` or `semanticcpg`. Language-specific logic stays in the frontend.
- A change in `semanticcpg`, `x2cpg` or `dataflowengineoss` affects many modules. Keep these APIs small and stable.
  Joern is used as a library by other projects (for example via `io.joern:*` on Maven Central).
- Do not make a module depend on a module that is further out, see [architecture](01_architecture.md).
- Expose as little as possible. Mark helpers `private` or `private[module]`.

Frontend conventions
--------------------

- Reuse the shared constants in `io.joern.x2cpg.Defines` (`Any`, `UnresolvedNamespace`, `UnresolvedSignature`,
  `ConstructorMethodName`, ...) rather than redefining them. Each frontend extends them in its own `Defines`.
- Prefer the standard operators (`io.shiftleft.codepropertygraph.generated.Operators`) for language constructs.
  Frontend-specific operators use the `<operator>.name` form, see `c2cpg`'s `Defines`.
- Mark unresolved method full names as described in the comment in `io.joern.x2cpg.Defines`, so that downstream
  passes can tell resolved from unresolved calls.
- Set `order`, `argumentIndex`, `lineNumber`, `columnNumber` and `code` on new AST nodes. Tests, the CFG and the
  data flow engine depend on them.
