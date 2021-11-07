[![Build Status](https://secure.travis-ci.org/ShiftLeftSecurity/fuzzyc2cpg.png?branch=master)](http://travis-ci.org/ShiftLeftSecurity/fuzzyc2cpg)
[![bintray](https://api.bintray.com/packages/shiftleft/maven/fuzzyc2cpg/images/download.svg)](https://bintray.com/shiftleft/maven/fuzzyc2cpg/_latestVersion)

# fuzzyc2cpg

**Note: for first-time users, we recommend building "joern" at https://github.com/ShiftLeftSecurity/joern/ instead. It contains both fuzzyc2cpg and a component for querying code property graphs, as well as a few helpful examples to get started.**

A fuzzy parser for C/C++ that creates code property graphs according to the specification at https://github.com/ShiftLeftSecurity/codepropertygraph . This is a fork of the (now unmaintainted) version of Joern at https://github.com/octopus-platform/joern.

## Using as a dependency
`build.sbt`:
```
libraryDependencies += "io.shiftleft" %% "fuzzyc2cpg" % "x.y.z"
resolvers += Resolver.bintrayRepo("shiftleft", "maven")
```
Other build tools: see [bintray instructions](https://bintray.com/shiftleft/maven/fuzzyc2cpg/_latestVersion).

## Building the code

The build process has been verified on Linux and it should be possible 
to build on OS X and BSD systems as well. The build process requires
the following prerequisites:

* Java runtime 11
  - Link: http://openjdk.java.net/install/
* Scala build tool (sbt)
  - Link: https://www.scala-sbt.org/

Additional build-time dependencies are automatically downloaded as part
of the build process. To build fuzzyc2cpg issue the command `sbt stage`.

Optionally, you may also build the preprocessor included with fuzzyc2cpg. This will enable you to specify any include
files that your application uses for more complete CPG generation. Any defines and macros will also be fully processed.

To build the preprocessor, see the preprocessor [README](./fuzzypp/README.md)/

## Running

To produce a code property graph _*without preprocessing*_  issue the command:
```shell script
./fuzzyc2cpg.sh <path/to/sourceCodeDirectory> --output <path/to/outputCpg>
`````

To produce a code property graph _*with preprocessing*_, ensure that you have the preprocessor binary available
and issue the command:
```shell script
./fuzzyc2cpg.sh <path/to/sourceCodeDirectory> \
                --verbose
                --source-file-ext .cxx 
                --output <path/to/outputCpg> \
                --include <path/to/include/file.h>
                -I <path/to/include/dir>
                --define DEF
                --define DEF_VAL=2
                --undefine UNDEF
                --preprocessor-executable <path/to/preprocessor/executable>
```

All preprocessor options may be specified more than once, with the exception of `--output` and `--preprocessor-executable`.
By default, fuzzyc2cpg will attempt to execute the preprocessor at `./fuzzypp/bin/fuzzyppcli`.

Run the following to see a complete list of available options:
```shell script
./fuzzyc2cpg.sh --help
```
