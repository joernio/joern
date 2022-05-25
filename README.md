Joern - The Bug Hunter's Workbench
===

[![release](https://github.com/joernio/joern/actions/workflows/release.yml/badge.svg)](https://github.com/joernio/joern/actions/workflows/release.yml)
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

## Requirements

- JDK 11 (newer versions _might_ work, but have not been properly tested)
- _optional_: gcc and g++ (for auto-discovery of C/C++ system header files if included/used in your C/C++ code)

## Quick Installation

```
wget https://github.com/joernio/joern/releases/latest/download/joern-install.sh
chmod +x ./joern-install.sh
sudo ./joern-install.sh
joern

Compiling (synthetic)/ammonite/predef/interpBridge.sc
Compiling (synthetic)/ammonite/predef/replBridge.sc
Compiling (synthetic)/ammonite/predef/DefaultPredef.sc
Compiling /home/tmp/shiftleft/joern/(console)

     ██╗ ██████╗ ███████╗██████╗ ███╗   ██╗
     ██║██╔═══██╗██╔════╝██╔══██╗████╗  ██║
     ██║██║   ██║█████╗  ██████╔╝██╔██╗ ██║
██   ██║██║   ██║██╔══╝  ██╔══██╗██║╚██╗██║
╚█████╔╝╚██████╔╝███████╗██║  ██║██║ ╚████║
 ╚════╝  ╚═════╝ ╚══════╝╚═╝  ╚═╝╚═╝  ╚═══╝

joern>

```

If the installation script fails for any reason, try
```
./joern-install --interactive
```

## QueryDB (queries plugin)
Quick way to develop and test QueryDB:
```
sbt stage
./querydb-install.sh
./joern-scan --list-query-names
```
The last command prints all available queries - add your own in querydb, run the above commands again to see that your query got deployed.
More details in the [separate querydb readme](querydb/README.md)

## Javasrc2cpg (a source-based frontend for Java)
See details in [the javasrc2cpg readme](joern-cli/frontends/javasrc2cpg/README.md)

## Benchmarks

Various static analysis benchmarks that measure Joern are contained under the `benchmarks`. The benchmarks are 
implemented in ScalaTest and can be run using the `joern-benchmarks` script. The benchmark results can be found on 
the `benchmarks` subproject's `README`. The currently implemented benchmarks along with the language frontends tested 
are:

* [Securibench Micro](http://too4words.github.io/securibench-micro/) [`javasrc2cpg`, `jimple2cpg`]
* [IFSpec](https://link.springer.com/chapter/10.1007/978-3-030-03638-6_27) [`javasrc2cpg`, `jimple2cpg`]

For more instructions on how to run benchmarks individually head over to the `benchmarks` subproject. If you would
like the benchmark results to be written to a file instead of printed to STDOUT, set the path to the environment 
variable `JOERN_BENCHMARK_RESULT_FILE`.
