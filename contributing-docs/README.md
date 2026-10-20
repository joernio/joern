Contributor's guide
===================

Contributions are welcome and are greatly appreciated! There are lots of ways to contribute to Joern,
depending on your interests and skill level.

- Report bugs with a minimal reproducer, ideally a small code snippet in the affected language.
- Fix a frontend bug: wrong AST, CFG, types or call resolution for some language construct.
- Improve data flow analysis.
- Add or improve queries in the query database.
- Improve the documentation.
- Review pull requests.

Contents
--------

1. [Architecture](01_architecture.md): modules, how they depend on each other, what a frontend is.
2. [Development environment](02_development_environment.md): JDK, sbt, external tools, IDE setup.
3. [Coding style](03_coding_style.md): scalafmt, scalafix rules, conventions.
4. [Unit tests](04_unit_tests.md): test fixtures, running tests, writing tests.
5. [Integration tests](05_integration_tests.md): end-to-end tests of the console and the distribution.
6. [Manual tests](06_manual_tests.md): staging Joern and trying a change by hand.
7. [Tech stack](07_tech_stack.md): the libraries and tools behind each module.
8. [Pull requests](08_pull_requests.md): what a good PR looks like, and what CI checks.
9. [Common tasks](09_common_tasks.md): add a query, pass, frontend; update dependencies.
99. [Hints](99_hints.md): pitfalls and tidbits.

Other documents
---------------

- [README](../README.md): installation and usage.
- [bazel.md](../bazel.md): the experimental Bazel build.
- [querydb/README.md](../querydb/README.md): writing queries for the query database.
- [changelog/](../changelog): migration notes for breaking changes.

Reporting issues
----------------

Use the [issue templates](https://github.com/joernio/joern/issues/new/choose). Please keep the conversation in
English, and share your input code or a minified example. That makes it much easier for us to help.

For AI coding agents
--------------------

The entry point for agents is [AGENTS.md](../AGENTS.md) in the repository root. `CLAUDE.md` is a symlink to it.
Keep `AGENTS.md` short, and put details in this directory.
