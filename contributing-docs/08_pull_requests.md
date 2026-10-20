Pull requests
=============

Thank you for taking the time to contribute to Joern! This page describes what a pull request (PR) should look like,
and what CI checks, so that your PR can be merged quickly.

Before you start
----------------

- Check the [issue tracker](https://github.com/joernio/joern/issues) for an existing issue, or open one.
  For a larger change, discuss it in an issue or on [Discord](https://discord.com/invite/vv4MH284Hc) first.
- For a bug fix, **reproduce the problem first**, ideally as a failing unit test.
- Read the [architecture](01_architecture.md) to find the module that owns the problem.

Checklist
---------

- [ ] The PR title starts with the affected module in square brackets: `[javasrc2cpg] Fix addition operator`.
      Use `[build]`, `[CI]` or `[bazel]` for changes to the build, the workflows and the Bazel setup, and `[all]` for
      changes across many modules.
- [ ] The description says what changed and why, in a few bullet points or paragraphs, and links the related
      issue. Do not leave the description empty.
- [ ] A unit test verifies the change. For a bug fix it fails without the fix. See [unit tests](04_unit_tests.md).
- [ ] The code is formatted: `sbt "scalafmt ; Test/scalafmt"`.
- [ ] Scalafix is clean for your changes:
      `sbt "scalafix --diff-base origin/master RestrictedImports SingleLetterIdentifiers UnorderedIteration"`.
- [ ] The affected module compiles without warnings, and its tests pass: `sbt {module}/test`.
- [ ] If you changed `semanticcpg`, `x2cpg` or `dataflowengineoss`: the tests of the modules that depend on it pass.
- [ ] If you changed dependencies: `Versions.scala` is updated, and `MODULE.bazel` and `maven_install.json` if the
      module has a Bazel `BUILD` file, see [common tasks](09_common_tasks.md).
- [ ] If you made a breaking change for users of the console or the library: add a note to [changelog/](../changelog).

Keep a PR focused. A PR that does one thing is reviewed faster than a large one. Do not include unrelated formatting
changes or refactorings.

Commit messages
---------------

The commit message (and the squashed PR title) follows the same shape as the PR title, and ends with the PR
number that GitHub adds:

```
[c2cpg] Do not drop `= delete` / `= default` on special members (#6444)
```

- Start with the module in square brackets.
- Use the imperative, and describe what changes, not how.

What CI checks
--------------

The [PR workflow](../.github/workflows/pr.yml) runs the following jobs. All of them are required.

| Job | What it does | Reproduce locally |
|-----|--------------|-------------------|
| `lint-and-format` | `scalafmtCheck`, `Test/scalafmtCheck` (also for `integrationTests`), and scalafix on the diff against the base branch | see checklist above |
| `validate-citation` | Validates [CITATION.cff](../CITATION.cff) | n/a |
| `run-tests` | `sbt testFull` on Linux, macOS and Windows | `sbt {module}/testFull` |
| `integration-tests` | `sbt "c2cpg/stage ; integrationTests/testFull"` on all three platforms | [integration tests](05_integration_tests.md) |
| `test-scripts` | Builds the distribution and runs [testDistro.py](../testDistro.py) on all three platforms | [integration tests](05_integration_tests.md) |

When a job fails on a platform you do not have, read the log first. Typical causes are path separators, line
endings, an unordered collection, or a tool that is missing from the runner.

The [master workflow](../.github/workflows/master.yml) runs the same checks after the merge. A
[release](../.github/workflows/release.yml) is created automatically once per day, so a merged PR is released
soon after.

Review
------

- Do not CC or @-mention other contributors right after you opened the PR. The team will review it and assign the
  most appropriate reviewer. Joern is maintained by industry partners and researchers with their own priorities, and
  additional help is largely volunteer work.
- If your PR goes stale, follow up in a comment and ask about the priority.
- Respond to review comments with a new commit, and mention what you changed. The PR is squashed on merge.

AI-assisted changes
-------------------

The same rules apply to changes written with the help of an AI tool. You are responsible for what you submit:
read and understand every line, run the checks above yourself, and do not submit code you cannot explain.
Do not reproduce copyrighted code.
