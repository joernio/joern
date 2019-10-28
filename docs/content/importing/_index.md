+++
title="Importing Code"
weight=2
+++

Code can be imported into joern in two ways. The program `joern-parse`
can be used to create Code Property Graphs and persist them in a
format based on Google Protocol Buffer messages, or, `create-cpg` can
be used to instruct joern-server to create a code property graph and
make it available via the REST API.

## Creating Code Property Graphs with `joern-parse`

Code property graphs can be created from C/C++ code using
`joern-parse`. For example, to create a Code Property Graph for the
vulnerable sample program `tarpit-c`, you can issue the following
commands:

```bash
git clone https://github.com/ShiftLeftSecurity/tarpit-c
./joern-parse tarpit-c
```

This will create a file named `cpg.bin.zip` in the current working
directory from the C/C++ code in the directory `tarpit-c`.

Joern-parse has the following signature:

```bash
./joern-parse <path/to/directory> --out <path/to/cpg/cpg_name>
```

It requires an input directory to be specified as a first positional
argument. Optionally, it allows the output location to be specified
via the `--out` flag.

```bash
./joern-parse tarpit-c --out tarpitc.bin.zip
```

will write the CPG to `tarpitc.bin.zip`.

If you omit the ```--out``` flag, the CPG is named `cpg.bin.zip` and
stored in the current working directory.

To view all options offered by `fuzzyc2cpg`, simply run
```bash
./joern-parse
```

## Creating complete Code Property Graphs
What happens with header files included in your programs? By default, unless you explicitly copy any used headers into
your source, they will be ignored by `joern-parse`, resulting in a valid, but incomplete CPG. 

To solve this, we can use the [FuzzyC2CPG preprocessor](https://github.com/ShiftLeftSecurity/fuzzyc2cpg/#running). 
Using this tool, you can specify the location of any header files and defines/macros used by your source code. These 
will then be used by Joern to generate a more complete CPG. Any missing header files will simply be elided from the 
resulting CPG.

For a more detailed explanation of the preprocessor options available to you, pass the `--help` option to
`joern-parse`.

## Creating Code Property Graphs with `cpg-create`

Make sure that `joern-server` is installed in `$JOERN-SERVER-DIR` and
that `cpgclientlib` is installed. Next, ensure that the joern server
is running. If it is not, you can start it as follows:

```bash
cd $JOERN-SERVER-DIR
./joernd
```

In another terminal, issue the following command:

```bash
git clone https://github.com/ShiftLeftSecurity/tarpit-c
cpg-create tarpit-c
```

This will parse the code in the directory `tarpit-c`, create a code
property graph on, and make it available via the joern server.
