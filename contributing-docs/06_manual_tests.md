Manual tests
============

Automated tests cover most changes. For a frontend or a console change it is still worth trying the result by hand,
on a real project, before you open a pull request. This page shows how.

Stage the distribution
----------------------

```bash
sbt joerncli/stage
```

This builds all frontends, the console and the tools, and puts them into `joern-cli/target/universal/stage/`:

```bash
cd joern-cli/target/universal/stage
./joern                          # interactive console
./joern-parse /path/to/code      # create a CPG (cpg.bin)
./joern-scan /path/to/code       # run the query database
./c2cpg.sh /path/to/code -o /tmp/cpg.bin  # run one frontend directly
```

Stage only what you need, it is much faster:

```bash
sbt c2cpg/stage                  # a single frontend
sbt javasrc2cpg/stage
```

A staged frontend can be started from its own directory, for example `joern-cli/frontends/c2cpg/c2cpg.sh`:

```bash
joern-cli/frontends/c2cpg/c2cpg.sh /path/to/code --output /tmp/cpg.bin
```

Frontend options are described in each frontend's `README.md`, or by `--help`.

Try a change in the console
---------------------------

```bash
joern-cli/target/universal/stage/joern
```

```scala
joern> importCode("/path/to/code")
joern> cpg.method.name.l
joern> cpg.call.name("strcpy").argument.l
joern> run.ossdataflow
joern> def source = cpg.method.name("getenv").callIn
joern> def sink   = cpg.call.name("system").argument
joern> sink.reachableByFlows(source).p
```

Use `help` in the console to see all commands. To run a script instead of an interactive session:

```bash
joern-cli/target/universal/stage/joern --script tests/test-simple.sc
```

Inspect a graph
---------------

- Print an AST, CFG or PDG: `cpg.method.name("main").dotAst.l`, `.dotCfg.l`, `.dotPdg.l`.
- Export a whole CPG with `joern-export`.
- Compare the CPG before and after your change on the same input. The output must be deterministic, so the same
  input and the same code must give the same CPG.

Try the query database
----------------------

```bash
sbt stage
./querydb-install.sh
./joern-scan --list-query-names
```

See the [querydb README](../querydb/README.md) for details.

Reproduce a bug report
----------------------

1. Save the reporter's code in a temporary directory, as small as possible.
2. Stage the frontend, run it and look at the CPG.
3. Write a unit test that fails for the same reason. Then fix the code until it passes.
4. Run the manual test again on the original code.

Test on real code
-----------------

For changes that affect many inputs, parse a few real open source projects of the language before and after your
change. Check that:

- the frontend does not crash, and does not log new errors or warnings,
- the number of methods, calls and types looks reasonable,
- the run time did not get noticeably worse.

Clean up
--------

Joern creates a `workspace` directory in the working directory, and `cpg.bin` files. These are git-ignored, but
they use disk space. Remove them when you are done.
