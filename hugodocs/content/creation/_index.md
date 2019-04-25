+++
title="Creation"
+++

## Code Property Graph Creation

Joern includes [fuzzyc2cpg](https://github.com/ShiftLeftSecurity/fuzzyc2cpg), a fuzzy C/C++ language module, which allows code property graphs to be created from C/C++ code. To use it, run `./fuzzyc2cpg.sh <path/to/directory> --out <path/to/cpg/cpg_name>`. If you ommit the ```--out``` flag, the CPG is named `cpg.bin.zip` and stored in the local folder.

As an example, run
```
./fuzzyc2cpg.sh tests/free
```
to create a CPG for the test project `free`.
