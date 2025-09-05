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

jssrc2cpg uses [joernio/astgen](https://github.com/joernio/astgen) under the hood.
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
> ./jssrc2cpg.sh --help

Usage: jssrc2cpg [options] input-dir

  input-dir                source directory
  -o, --output <value>     output filename
  --exclude <file1>        files or folders to exclude during CPG generation (paths relative to <input-dir> or absolute paths)
  --exclude-regex <value>  a regex specifying files to exclude during CPG generation (paths relative to <input-dir> are matched)
  --enable-early-schema-checking
                           enables early schema validation during AST creation (disabled by default)
  --enable-file-content    add the raw source code to the content field of FILE nodes to allow for method source retrieval via offset fields (disabled by default)
  --help                   display this help message
```

## Alternative with Transpiling

You may want to use https://github.com/ShiftLeftSecurity/js2cpg powered by the [GraalJS parser](https://github.com/oracle/graaljs/tree/master/graal-js/src/com.oracle.js.parser) which is part of the [GraalVM JS project](https://www.graalvm.org/latest/reference-manual/js/).

js2cpg attempts to run several transpilers / preprocessors if the input project contains at least one element of the targeted language extension or template language (e.g., at least one Typescript file).

This includes:
  - Babel
  - EJS
  - Nuxt.js
  - PUG templates
  - Vue.js templates
  - Typescript

Transpiling ensures we have ES6 compliant JS code before we continue with the actual parsing and CPG-generation.
