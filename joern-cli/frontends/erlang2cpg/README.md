# erlang2cpg

Import Erlang source with Joern and Erlang/OTP 27. Install a JDK (CI uses 21) and put OTP 27's `escript` on PATH. Building from this checkout also requires sbt and access to its dependency repositories.

```sh
sbt erlang2cpg/test joerncli/stage
export PATH="$PWD/joern-cli/target/universal/stage:$PATH"
fixture="$PWD/joern-cli/frontends/erlang2cpg/integration/fixture"
cd "$(mktemp -d)"
joern-parse --list-languages
joern-parse --language ERLANG "$fixture" --output explicit.cpg
joern-parse "$fixture" --output detected.cpg
joern
```

In the console, open the saved graph with `importCpg("explicit.cpg")`. For a fresh source import use `importCode("/absolute/path/to/project")` or `importCode.erlang("/absolute/path/to/project")`. The following queries use the two-module fixture.

```scala
cpg.method.isExternal(false).fullName.l.sorted
cpg.method.fullNameExact("demo:route/1").call.nameExact("keep").callee.fullName.l
cpg.method.fullNameExact("demo:through/2").call.nameExact("observe").argument(1)
  .reachableByFlows(cpg.method.fullNameExact("demo:through/2").parameter.index(2)).p
```

Pass a single `.erl` file or a directory. Directory imports recursively discover modules. Each module starts a fresh OTP preprocessor with its own macro environment. Included headers contribute forms and source locations to the including module. To supply includes or macros, append `--frontend-args --include /path/to/include --define FEATURE --define LIMIT=32`. Both options are repeatable. Macro values use Erlang literal syntax. OTP resolves `-include_lib` against available applications. Imports read source without compiling the project or running build hooks, target modules or their parse transforms.

A preprocessing or syntax error skips that module with a diagnostic. Valid siblings remain available. Duplicate module names abort the import with the conflicting paths. Use the standard frontend exclusion options to omit duplicate build trees. If every module fails, or the helper cannot run, the frontend exits unsuccessfully. The CLI frontend publishes its output after closing the graph successfully, preserving an existing output on translation failure.

Method identities use `module:function/arity`. A dynamic module or function expression retains its operands. Process operations model local values and control flow. The graph does not infer mailbox delivery, spawn scheduling or behaviour dispatch. Unsupported executable forms appear as UNKNOWN nodes with their OTP form kind and location, and the frontend logs a per-file count.

The renderer normalizes expression text and omits source trivia. Locations are one-based where epp supplies them. Case and if expressions expose bindings shared by every clause. The frontend approximates bindings across receive and exception-handling branches. Exception transfer in a multi-expression try body uses the standard Joern CFG approximation.

To run the external CLI checks, obtain `lib/stdlib/src/proplists.erl` from OTP commit `05737d130706c7189a8e6750d9c2252d2cc7987e` (tag `OTP-27.3`). From the repository root run:

```sh
python3 joern-cli/frontends/erlang2cpg/integration/verify.py \
  joern-cli/target/universal/stage --proplists /absolute/path/to/proplists.erl
```
