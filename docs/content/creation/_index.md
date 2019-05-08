+++
title="Creating Code Property Graphs"
weight=2
+++

Joern includes
[fuzzyc2cpg](https://github.com/ShiftLeftSecurity/fuzzyc2cpg), a fuzzy
C/C++ language module, which allows code property graphs to be created
from C/C++ code. To use it, run

```
./joern-parse <path/to/directory> --out <path/to/cpg/cpg_name>
```

If you ommit the ```--out``` flag, the CPG is named `cpg.bin.zip` and stored in the local folder.

As an example, run
```
./joern-parse tests/free
```
to create a CPG for the test project `free`.

To view all options offered by `fuzzyc2cpg`, simply run
```
./joern-parse
```

You can also programmatically create CPGs as demonstrated by the following test.

{{<snippet file="src/test/scala/io/shiftleft/joern/GenerationTests.scala" language="scala">}}
