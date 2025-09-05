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

swiftsrc2cpg uses [joernio/swiftastgen](https://github.com/joernio/swiftastgen) under the hood.
Native binaries for Linux, macOS, and Windows are generated as described [here](https://github.com/joernio/swiftastgen#building).
To build your own native binaries run the following commands:

```shell script
git clone https://github.com/joernio/swiftastgen.git
cd swiftastgen
swift build
```
(requires `swift`).

## Running

### Required Runtime Dependencies:
 - Java runtime >= 11
 - `glibc` >= 2.34 (for swiftastgen); or `swift` [5.10](https://www.swift.org/install) directly which brings in all required dependencies anyway.

### Manual Installation for Swift via Tarball (e.g., for Ubuntu 20.04)
 - ```
   $ apt-get install \
          binutils \
          git \
          gnupg2 \
          libc6-dev \
          libcurl4 \
          libedit2 \
          libgcc-9-dev \
          libpython2.7 \
          libsqlite3-0 \
          libstdc++-9-dev \
          libxml2 \
          libz3-dev \
          pkg-config \
          tzdata \
          uuid-dev \
          zlib1g-dev
   ```
 - and follow the steps in https://www.swift.org/install

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
> ./swiftsrc2cpg.sh --help

Usage: swiftsrc2cpg [options] input-dir

  input-dir                source directory
  -o, --output <value>     output filename
  --exclude <file1>        files or folders to exclude during CPG generation (paths relative to <input-dir> or absolute paths)
  --exclude-regex <value>  a regex specifying files to exclude during CPG generation (paths relative to <input-dir> are matched)
  --enable-early-schema-checking
                           enables early schema validation during AST creation (disabled by default)
  --enable-file-content    add the raw source code to the content field of FILE nodes to allow for method source retrieval via offset fields (disabled by default)
  --help                   display this help message
  --define <value>         define a name
  --swift-build            build the project to retrieve full Swift compiler type information (requires Swift > 6.1)
  --build-log-path <value>
                           the path to the compiler debug output log file (this enables --swift-build; requires Swift > 6.1)
```

### Optional Dependencies

Extraction of type, declaration, and call fullnames via the Swift compiler is enabled by invoking swiftsrc2cpg with the `--swift-build` flag.
This process attempts to compile the input project as a SwiftPM package using `swift build`.

For projects requiring custom-builds outside SwiftPM (e.g., `Xcode` or `make`), capture the build output (including all invocations of `swiftc`)
to a file and provide it to swiftsrc2cpg using the `--build-log-path <path>` option.

_Note_: This functionality requires Swift version 6.1 or later, which introduces the `--dump-ast-format json` compiler flag.
