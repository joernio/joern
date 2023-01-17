# jssrc2cpg

A Babel based parser for JavaScript/TypeScript that creates code property graphs according to the specification at https://github.com/ShiftLeftSecurity/codepropertygraph .

## Building the code

The build process has been verified on Linux, and it should be possible 
to build on OS X and BSD systems as well. The build process requires
the following prerequisites:

* Java runtime 11
  - Link: http://openjdk.java.net/install/
* Scala build tool (sbt)
  - Link: https://www.scala-sbt.org/

Additional build-time dependencies are automatically downloaded as part
of the build process. To build jssrc2cpg issue the command `sbt stage`.

## JS/TS AST Generation

jssrc2cpg uses [@joern/astgen](https://github.com/joernio/astgen) under the hood.
Native binaries for Linux, macOS, and Windows are generated as described [here](https://github.com/joernio/astgen#building).
To build your own native binaries run the following commands:

```shell script
git clone https://github.com/joernio/astgen.git
cd astgen
yarn install
```
(requires `yarn`).

Copy the resulting `astgen-linux`, `astgen-macos`, `astgen-macos-arm`, and `astgen-win.exe` to `joern/joern-cli/frontends/jssrc2cpg/bin/astgen`.

## Running

To produce a code property graph  issue the command:
```shell script
./jssrc2cpg.sh <path/to/sourceCodeDirectory> --output <path/to/outputCpg>
`````

Run the following to see a complete list of available options:
```shell script
./jssrc2cpg.sh --help
```

## Warning

This is work in progress. Use https://github.com/ShiftLeftSecurity/js2cpg as a mature alternative.

