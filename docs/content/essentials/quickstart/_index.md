+++
title="Quick Start"
weight=1
+++

Clone and build the Joern server:

```
git clone https://github.com/ShiftLeftSecurity/joern.git
cd joern
sbt stage
```

Install Python-based utilities:

```
pip install cpgclientlib
```

Alternatively, you can install the newest version of `cpgclientlib` from the `codepropertygraph` repository:

```
git clone https://github.com/ShiftLeftSecurity/codepropertygraph.git
cd codepropertygraph/cpgclientlib
sudo python setup.py install
```

Once `cpgclientlib` is installed, start the server:

```
./joernd
```

Create a CPG for a sample program and query it to retrieve all methods in JSON format:

```
cpg-create joern-cli/src/test/resources/testcode/free
cpg-query "cpg.method.toJson"
```
