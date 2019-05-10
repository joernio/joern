+++
title="Querying Graphs"
weight=2
+++

## Quick Start

One you have [created a code property graph](/docs/creation), you can
load and query it with `joern-query`:

```
./joern-query <query>
```

For example, you can run
```
./joern-query cpg.method.name
```

to obtain the names of all methods. By default, the code property
graph at `cpg.bin.zip` is loaded. You can specify an alternative CPG
using the `-c` flag. For example,

```
./joern-query -c path/to/my/cpg.bin.zip cpg.method.name
```
will process the CPG at `path/to/my/cpg.bin.zip`.

## Passing scripts to `joern-query`

Instead of specifying a query on the shell, you can also pass scripts
to `joern-query` as follows:

```
./joern-query -f myscript.scala
```

To list all methods via a script, try the following:

```
echo "cpg.method.name.l.mkString(\"\\n\")" > foo.scala
./joern-query -f foo.scala 
```
