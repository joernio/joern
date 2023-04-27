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

<!-- drop in a few months, e.g. end of 2023 -->
## Announcement: upgrading from Joern 1.x to 2.x: see notes [below](#12x-upgrade-to-scala-3)

## Requirements

- JDK 19 (other versions _might_ work, but have not been properly tested)
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

## Developers: IDE setup

### Intellij IDEA
* [Download Intellij Community](https://www.jetbrains.com/idea/download)
* Install and run it
* Install the [Scala Plugin](https://plugins.jetbrains.com/plugin/1347-scala) - just search and install from within Intellij
* Important: run `sbt` in your local joern clone and keep it open - this will allow us to use the BSP build in the next step
* Back to Intellij: open project: select your local joern clone: select to open as `BSP project` (i.e. _not_ `sbt project`!)
* Await the import and indexing to complete, then you can start, e.g. `Build -> build project` or run a test

### VSCode
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

## Benchmarks

Various static analysis benchmarks that measure Joern are contained under the `benchmarks`. The benchmarks are
implemented in ScalaTest and can be run using the `joern-benchmarks` script. The benchmark results can be found on
the `benchmarks` subproject's `README`. The currently implemented benchmarks along with the language frontends tested
are:

* [Securibench Micro](http://too4words.github.io/securibench-micro/) [`javasrc2cpg`, `jimple2cpg`]
* [IFSpec](https://link.springer.com/chapter/10.1007/978-3-030-03638-6_27) ([paper](https://pp.ipd.kit.edu/uploads/publikationen/ifspec18nordsec.pdf)) [`javasrc2cpg`, `jimple2cpg`]
* [JInfoFlow](https://github.com/plast-lab/JInfoFlow-bench) ([paper](https://yanniss.github.io/ptaint-oopsla17-prelim.pdf)) [`javasrc2cpg`, `jimple2cpg`]

For more instructions on how to run benchmarks individually head over to the `benchmarks` subproject. If you would
like the benchmark results to be written to a file instead of printed to STDOUT, set the path to the environment
variable `JOERN_BENCHMARK_RESULT_FILE`.

## Upgrade notes

### 2.0.x: Upgrade to Scala 3
Joern is based on Scala. As of Joern 2.0.x we upgraded from Scala 2 to Scala 3. 
This is a major version upgrade and Scala 3 is essentially a new language with a new REPL implementation, so this may sound scary. 

That being said, both the Scala as well as Joern maintainers have made an effort to minimize changes to the API, in order to ease the transition for users. Most importantly, the Joern workspace DSL (`importCode(...)` etc.) and the CPG Traversal DSL (e.g. `cpg.method.name("foo").l`) are unchanged. The latter is based on Scala collections API, which is actually identical (a shared library) between Scala 2 and Scala 3. 

Depending on your use case you may not even notice a difference: we've tried to keep as much as possible just like it was - most importantly the query DSL.
There are however a few changes - some that we believe are the better, and some were just unavoidable. Here's the most important ones as far as I can tell:

1) The 'import a script' magic `$file.foo` from Ammonite was replaced by the `//> using file foo.sc` directive. This works in an active joern REPL as well as in scripts.

2) Adding dependencies: the magic `$ivy.my-dependency` was replaced:
* `--dep` parameter, e.g. `./joern --dep com.michaelpollmeier:versionsort:1.0.7`. Can be specified multiple times.
* For scripts there's a slightly nicer alternative that let's you specify your dependencies within the script itself: `//> using dep com.michaelpollmeier:versionsort:1.0.7`
* all dependencies need to be known when joern starts, i.e. you can not dynamically add more dependencies to an active joern REPL session

3) Script parameters: pass multiple `--param` parameters rather than one comma-separated list. Example:
```
// old
./joern --script foo.sc --params paramA=valueA,paramB=valueB
// new
./joern --script foo.sc --param paramA=valueA --param paramB=valueB
```
While that's slightly longer it is also less complex, easier to read, and you can pass values that contain commas :)

Apart from that, Scala 3 is a bit stricter when it comes to adding or leaving out `()` for function application. The compiler messages are (on average) much better than before, so hopefully it'll guide you as good as possible. 

Depending on your level of integration with Joern you might not even notice anything. If you do, please check the lists above and below, and if that doesn't help: open a [github issue](https://github.com/joernio/joern/issues/new) or hit us up on [discord](https://discord.gg/vv4MH284Hc).

Some more generic Scala-issues when upgrading Scala 2 to Scala 3:

1. anonymous functions need an extra parenthesis around their parameter list:
```scala
Seq(1,2,3).map { i: Int => i + 1 }   
// error: parentheses are required around the parameter of a lambda

// option 1: add parentheses, as suggested by compiler:
Seq(1,2,3).map { (i: Int) => i + 1 }

// option 2: drop type annotation (if possible):
Seq(1,2,3).map { i => i + 1 }
```

2. `main` entrypoint: `def main` instead of `extends App`
See https://docs.scala-lang.org/scala3/book/methods-main-methods.html
```scala
object Main extends App {
  println("hello world")
}

// depending on usage, may lead to NullPointerExceptions
// context: Scala3 doesn't support the 'magic' DelayedInit trait

// rewrite to:
object Main {
  def main(args: Array[String]) = {
    println("hello world")
  }
}
```



  
