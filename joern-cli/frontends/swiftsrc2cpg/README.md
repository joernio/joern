# swiftsrc2cpg

A Swift Syntax based parser for Swift that creates code property graphs according to the specification at https://github.com/ShiftLeftSecurity/codepropertygraph .

## Building the code

The build process has been verified on Linux, and it should be possible 
to build on OS X and BSD systems as well. The build process requires
the following prerequisites:

* JDK 11
  - Link: http://openjdk.java.net/install/
* Scala build tool (sbt)
  - Link: https://www.scala-sbt.org/

Additional build-time dependencies are automatically downloaded as part
of the build process. To build swiftsrc2cpg issue the command `sbt stage`.

## Swift AST Generation

swiftsrc2cpg uses [@joern/swiftastgen](https://github.com/joernio/swiftastgen) under the hood.
Native binaries for Linux, macOS, and Windows are generated as described [here](https://github.com/joernio/swiftastgen#building).
To build your own native binaries run the following commands:

```shell script
git clone https://github.com/joernio/swiftastgen.git
cd swiftastgen
swift build
```
(requires `swift`).

## Running

Required runtime dependencies:
 - Java runtime >= 11
 - `glibc` >= 2.34 (for swiftastgen); or `swift` 5.9 directly which brings in all required dependencies anyway.

To produce a code property graph issue the command:
```shell script
./swiftsrc2cpg.sh <path/to/sourceCodeDirectory> --output <path/to/outputCpg>
`````

Additional options are available:
```shell script
./swiftsrc2cpg.sh <path/to/sourceCodeDirectory> \
                --output <path/to/outputCpg> \
                --define DEF
                --define DEF_VAL=2
```

Run the following to see a complete list of available options:
```shell script
./swiftsrc2cpg.sh --help
```

## Warning

This is work in progress.
