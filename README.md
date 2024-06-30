Joern - The Bug Hunter's Workbench
===

[![release](https://github.com/joernio/joern/actions/workflows/release.yml/badge.svg)](https://github.com/joernio/joern/actions/workflows/release.yml)
[![Joern SBT](https://index.scala-lang.org/joernio/joern/latest.svg)](https://index.scala-lang.org/joernio/joern)
[![Github All Releases](https://img.shields.io/github/downloads/joernio/joern/total.svg)](https://github.com/joernio/joern/releases/)
[![Gitter](https://img.shields.io/badge/-Discord-lime?style=for-the-badge&logo=discord&logoColor=white&color=black)](https://discord.com/invite/vv4MH284Hc)

Joern is a platform for analyzing source code, bytecode, and binary
executables. It generates code property graphs (CPGs), a graph
representation of code for cross-language code analysis. Code property
graphs are stored in a custom graph database. This allows code to be
mined using search queries formulated in a Scala-based domain-specific
query language. Joern is developed with the goal of providing a useful
tool for vulnerability discovery and research in static program
analysis.

Website: https://joern.io

Documentation: https://docs.joern.io/

Specification: https://cpg.joern.io

## News / Changelog

- Joern v2.0.0 [upgrades from Scala2 to Scala3](changelog/2.0.0-scala3.md)
- Joern v1.2.0 removes the `overflowdb.traversal.Traversal` class. This change is not completely backwards compatible. See [here](changelog/traversal_removal.md) for a detailed writeup.

## Requirements

- JDK 21 (other versions _might_ work, but have not been properly tested)
- _optional_: gcc and g++ (for auto-discovery of C/C++ system header files if included/used in your C/C++ code)

## Development Requirements

- mvn https://maven.apache.org/install.html

## Quick Installation

```
wget https://github.com/joernio/joern/releases/latest/download/joern-install.sh
chmod +x ./joern-install.sh
sudo ./joern-install.sh
joern

     ██╗ ██████╗ ███████╗██████╗ ███╗   ██╗
     ██║██╔═══██╗██╔════╝██╔══██╗████╗  ██║
     ██║██║   ██║█████╗  ██████╔╝██╔██╗ ██║
██   ██║██║   ██║██╔══╝  ██╔══██╗██║╚██╗██║
╚█████╔╝╚██████╔╝███████╗██║  ██║██║ ╚████║
 ╚════╝  ╚═════╝ ╚══════╝╚═╝  ╚═╝╚═╝  ╚═══╝
Version: 2.0.1
Type `help` to begin

joern>
```

If the installation script fails for any reason, try
```
./joern-install --interactive
```

## Docker based execution

```
docker run --rm -it -v /tmp:/tmp -v $(pwd):/app:rw -w /app -t ghcr.io/joernio/joern joern
```

To run joern in server mode:

```
docker run --rm -it -v /tmp:/tmp -v $(pwd):/app:rw -w /app -t ghcr.io/joernio/joern joern --server
```

Almalinux 9 requires the CPU to support SSE4.2. For kvm64 VM use the Almalinux 8 version instead.
```
docker run --rm -it -v /tmp:/tmp -v $(pwd):/app:rw -w /app -t ghcr.io/joernio/joern-alma8 joern
```

## Releases
A new release is [created automatically](.github/workflows/release.yml) once per day. Contributers can also manually run the [release workflow](https://github.com/joernio/joern/actions/workflows/release.yml) if they need the release sooner.

## Developers

### Contribution Guidelines

Thank you for taking time to contribute to Joern! Here are a few guidelines to ensure your pull request will get merged as soon as possible:

* Try to make use of the templates as far as possible, however they may not suit all needs. The minimum we would like to see is:
    - A title that briefly describes the change and purpose of the PR, preferably with the affected module in square brackets, e.g. `[javasrc2cpg] Addition Operator Fix`.
    - A short description of the changes in the body of the PR. This could be in bullet points or paragraphs.
    - A link or reference to the related issue, if any exists.
* Do not:
    - Immediately CC/@/email spam other contributors, the team will review the PR and assign the most appropriate contributor to review the PR. Joern is maintained by industry partners and researchers alike, for the most part with their own goals and priorities, and additional help is largely volunteer work. If your PR is going stale, then reach out to us in follow-up comments with @'s asking for an explanation of priority or planning of when it may be addressed (if ever, depending on quality).
    - Leave the description body empty, this makes reviewing the purpose of the PR difficult.
* Remember to:
    - Remember to format your code, i.e. run `sbt scalafmt Test/scalafmt`
    - Add a unit test to verify your change.

### IDE setup

#### Intellij IDEA
* [Download Intellij Community](https://www.jetbrains.com/idea/download)
* Install and run it
* Install the [Scala Plugin](https://plugins.jetbrains.com/plugin/1347-scala) - just search and install from within Intellij.
* Important: open `sbt` in your local joern repository, run `compile` and keep it open - this will allow us to use the BSP build in the next step
* Back to Intellij: open project: select your local joern clone: select to open as `BSP project` (i.e. _not_ `sbt project`!)
* Await the import and indexing to complete, then you can start, e.g. `Build -> build project` or run a test

#### VSCode
- Install VSCode and Docker
- Install the plugin `ms-vscode-remote.remote-containers`
- Open Joern project folder in [VSCode](https://docs.microsoft.com/en-us/azure-sphere/app-development/container-build-vscode#build-and-debug-the-project)
  Visual Studio Code detects the new files and opens a message box saying: `Folder contains a Dev Container configuration file. Reopen to folder to develop in a container.`
- Select the `Reopen in Container` button to reopen the folder in the container created by the `.devcontainer/Dockerfile` file
- Switch to `scalameta.metals` sidebar in VSCode, and select `import build` in `BUILD COMMANDS`
- After `import build` succeeds, you are ready to start writing code for Joern

## QueryDB (queries plugin)
Quick way to develop and test QueryDB:
```
sbt stage
./querydb-install.sh
./joern-scan --list-query-names
```
The last command prints all available queries - add your own in querydb, run the above commands again to see that your query got deployed.
More details in the [separate querydb readme](querydb/README.md)
