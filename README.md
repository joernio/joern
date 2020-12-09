Joern
===

[![Build Status](https://github.com/ShiftLeftSecurity/joern/workflows/release/badge.svg)](https://github.com/ShiftLeftSecurity/joern)
[![Gitter](https://badges.gitter.im/joern-code-analyzer/community.svg)](https://gitter.im/joern-code-analyzer/community?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge)

Documentation on the usage of Joern and Joern server may be found on the Joern [microsite](https://docs.joern.io/home).

Getting started with Joern
---

1. Ensure you have version 8 or above of the Java Runtime Environment installed:

```
$ java -version
openjdk version "1.8.0_275"
OpenJDK Runtime Environment (build 1.8.0_275-8u275-b01-0ubuntu1~20.04-b01)
OpenJDK 64-Bit Server VM (build 25.275-b01, mixed mode)
```

2. Download the latest
   [release](https://github.com/ShiftLeftSecurity/joern/releases) zip file, e.g.:
```
$ curl -L -O https://github.com/ShiftLeftSecurity/joern/releases/download/v1.1.43/joern-cli.zip
```

3. Unzip the folder

```
$ unzip joern-cli.zip
```

4. From inside the folder, start Joern:

```
$ cd joern-cli && ./joern
Compiling (synthetic)/ammonite/predef/interpBridge.sc
Compiling (synthetic)/ammonite/predef/replBridge.sc
Compiling (synthetic)/ammonite/predef/sourceBridge.sc
Compiling (synthetic)/ammonite/predef/frontEndBridge.sc
Compiling (synthetic)/ammonite/predef/DefaultPredef.sc
Compiling /home/user/joern-cli/(console)
creating workspace directory: /home/user/joern-cli/workspace

     ██╗ ██████╗ ███████╗██████╗ ███╗   ██╗
     ██║██╔═══██╗██╔════╝██╔══██╗████╗  ██║
     ██║██║   ██║█████╗  ██████╔╝██╔██╗ ██║
██   ██║██║   ██║██╔══╝  ██╔══██╗██║╚██╗██║
╚█████╔╝╚██████╔╝███████╗██║  ██║██║ ╚████║
 ╚════╝  ╚═════╝ ╚══════╝╚═╝  ╚═╝╚═╝  ╚═══╝
```

Releases
---
Pre-built instances of the Joern command-line tools may be found [here](https://github.com/ShiftLeftSecurity/joern/releases).

To run the Joern tools, you must have version 8 or above of the Java Runtime Environment installed.

Migrations
---
Joern [1.1.1](https://github.com/ShiftLeftSecurity/joern/releases/tag/v1.1.1) introduced a major rearchitecture of the cpg query language (CPGQL) - please make sure to read [upgrade guide](https://docs.joern.io/upgrade-guides)
