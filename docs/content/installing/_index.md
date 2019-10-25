+++
title="Installation"
weight=1
+++

Joern currently consists of the following components.

* [joern-cli](https://github.com/ShiftLeftSecurity/joern/tree/master/joern-cli). Joern
  command line tools for creating and analyzing code  property
  graphs. This package includes the interactive Joern shell.

* [joern-server](https://github.com/ShiftLeftSecurity/joern/tree/master/joern-server). The
  Joern REST server. This allows you to make code property graphs
  accessible via HTTP.

* [cpgclientlib](https://github.com/ShiftLeftSecurity/codepropertygraph/tree/master/cpgclientlib). A
  Python library for interacting with the Joern REST  server. This
  package also includes command line tools for creating code property
  graphs on the server side, and subsequently querying them.

If you only want to use the interactive shell or integrate Joern into
your own JVM-based project, you only need joern-cli. If, however, you
plan to access code property graphs using Python scripts or via
command line tools, the joern-server and cpgclientlib are for you.


## Prerequisites

joern-cli and joern-server are Scala applications and should work on
systems offering a Java virtual machine, e.g., Microsoft Windows, Mac
OS X or Gnu/Linux. cpgclientlib is a Python 3 library.

* Java 8
  - Link: http://openjdk.java.net/install/

* Python3 (only if you need cpgclientlib)
  - Link: https://www.python.org/downloads/

All other dependencies are packaged with Joern and do not need to be
installed manually. Python3 and Java 8 are present in the official
package repositories of all major current Linux distributions and BSD
flavors.

## Installing Pre-Built Binaries

Pre-built binaries of joern-cli and joern-server are available at:

https://github.com/ShiftLeftSecurity/joern/releases/

To install the latest release, simply execute the following:

```bash
wget https://github.com/ShiftLeftSecurity/joern/releases/latest/download/joern-cli.zip
wget https://github.com/ShiftLeftSecurity/joern/releases/latest/download/joern-server.zip
unzip joern-cli.zip
unzip joern-server.zip
```

You can test your installation as follows:

```bash
cd joern-cli
./joern
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

Welcome to ShiftLeft Ocular/Joern

joern>
```

This should print the joern banner and drop you onto the interactive
shell.

Finally, the Python 3 library and utilities of `cpgclientlib` can be installed via pip:

```
pip install cpgclientlib
```

You should find the scripts `cpg-create` and `cpg-query` in your path
after installation.

## Building from Source Code

To build joern-cli and joern-server from source code, you need to
install the Scala build tool (sbt), which you can install by following
the instructions at https://www.scala-sbt.org/download.html. Any 1.x
version of sbt works as sbt downloads the correct version for building
joern as part of the build process.


Once the dependencies are installed, run

```bash
git clone https://github.com/ShiftLeftSecurity/joern.git
cd joern
sbt stage
```

This builds joern-cli and joern-server in the current directory. To
build the distribution (joern-cli.zip and joern-server.zip), you can
issue `sbt createDistribution`.

You can install the newest version of `cpgclientlib` from the source
code hosted at the `codepropertygraph` repository:

```bash
git clone https://github.com/ShiftLeftSecurity/codepropertygraph.git
cd codepropertygraph/cpgclientlib
sudo python setup.py install
```


## Configuring the JVM for Optimal Performance

Code analysis can require lots of memory, and unfortunately, the JVM does not pick up the available amount of memory by itself. While tuning Java memory usage is a discipline in its own right, it is usually sufficient to specify the maximum available amount of heap memory using the JVM's -Xmx flag. The easiest way to achieve this globally is by setting the environment variable _JAVA_OPTS as follows:

```
export _JAVA_OPTS="-Xmx$NG"
```

where $N is the amount of memory in gigabytes. You can add this line to your shell startup script, e.g., ~/.bashrc or ~/.zshrc.
