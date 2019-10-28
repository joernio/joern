Joern
===

[![Build Status](https://travis-ci.org/ShiftLeftSecurity/joern.svg?branch=master)](https://travis-ci.org/ShiftLeftSecurity/joern)

Documentation on the usage of Joern and Joern server may be found on the Joern [microsite](https://joern.io/docs/).

Releases
---
Pre-built instances of the Joern command-line tools may be found [here](https://github.com/ShiftLeftSecurity/joern/releases).

To run the Joern tools, you must have version 8 or above of the Java Runtime Environment installed.

Building
---
To create your own distribution of the Joern tools, ensure you have `sbt` installed and simply run 
`sbt createDistribution`.

If you are building Joern using macOS you will need to install the `greadlink` package:
```shell script
brew install coreutils
```
