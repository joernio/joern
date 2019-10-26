+++
title="The Joern Shell"
weight=2
+++

Joern lets you perform code analysis both interactively using a shell or with non-interactive scripts. Think of Joern and Ocular as interpreters for a domain-specific code analysis language.

## Using the Shell

You can start the shell by executing
```bash
./joern
```
Once in the shell, `loadCpg($filename)` can be used to load the CPG stored at filename. Subsequently, it can be accessed as `cpg`. For example, the following loads the CPG stored at "cpg.bin.zip", lists all methods, and pipes them to the file "/tmp/foo.txt":

```bash
loadCpg("cpg.bin.zip")
cpg.method.name.l |> "/tmp/foo.txt"
```

The shell has the following useful features:

* \<TAB\> for autocomplete
* \<UP\> to navigate through command history
* \<CTRL+r\> to search command history

Finally, it offers pipe operators to write results to files:

* |> "$filename" writes to file at $filename
* |>> "$filename" appends to file at $filename

## Running scripts

You can run scripts non-interactively using `joern`.
 
### From the outside

The `--script` option allows to execute scripts in a newly instantiated session.
 
For example,

```bash
./joern --script scripts/list-funcs.scala
```

executes the `list-funcs.scala` script included with Joern, which loads the CPG at `cpg.bin.zip`, and writes out all function names.

You can also pass arguments to scripts. For example, the following script accepts the filename of a CPG (`cpgFile`) and a filename of a result file (`outFile`):

```scala
@main def exec(cpgFile: String, outFile: String) = {
   loadCpg(cpgFile)
   cpg.method.name.l |> outFile
}
```

It can be called as follows:

```bash
./joern --script test.sc --params cpgFile=/fullpath/to/cpg.bin.zip,outFile=out.log
```

### During a running session with the ScriptManager

As an alternative to the `--script` option, during a running Joern session the ScriptManager provides some basic script handling.
It offers the following two methods:

```scala
def scripts(): List[ScriptDescription]
```

This will list all available scripts (`name`, `description`) that are stored in the `scripts/` folder of Joern.

```scala
def runScript(name: String): AnyRef
```

This will execute the script named `name` as already explained above.
