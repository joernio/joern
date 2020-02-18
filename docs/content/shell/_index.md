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
Once in the shell, `loadCpg($filename)` can be used to load the CPG stored at filename. Subsequently, it can be accessed as `cpg`. For example, the following loads the CPG stored at "cpg.bin", lists all methods, and pipes them to the file "/tmp/foo.txt":

```bash
loadCpg("cpg.bin")
cpg.method.name.l |> "/tmp/foo.txt"
```

The shell has the following useful features:

* \<TAB\> for autocomplete
* \<UP\> to navigate through command history
* \<CTRL+r\> to search command history

Finally, it offers pipe operators to write results to files:

* |> "$filename" writes to file at $filename
* |>> "$filename" appends to file at $filename

## Inline Code Browsing

The Joern shell allows you to read code associated with query
results. For any expression, you can simply query the code field. For
example, to review all calls to `memcpy`, you can issue:

```bash
joern> cpg.method.name("memcpy").callIn.code.l

res5: List[String] = List(
  "memcpy(buf, first, first_len)",
  "memcpy(buf + first_len, second, second_len)",
  "memcpy(buf, first, first_len)",
  "memcpy(buf + first_len, second, second_len)",
  "memcpy(buf + first_len, second, second_len)",
  "memcpy(buf, first, first_len)"
)
```

You can also pipe the result list into a pager as follows:

```bash
joern> browse(cpg.method.name("memcpy").callIn.code.l)
```

**Please make sure [source-highlight](https://www.gnu.org/software/src-highlite/) is installed for the `.dump` feature to work.**

To study the context in which a result occurs, you can use the `.dump`
method, which will dump the enclosing function's code for each
finding, and point you to the finding via an arrow:

```bash
joern> cpg.method.name("memcpy").callIn.dump

int main() {
  unsigned int first_len = UINT_MAX - 256;
  unsigned int second_len = 256;
  unsigned int buf_len = 256;

  char first[first_len], second[second_len], buf[buf_len];
  int new_len = (first_len+second_len); // <- IDB (negative)

  if(new_len <= 256) {
	memcpy(buf, first, first_len);
        memcpy(buf + first_len, second, second_len); /* <=== */ 
  }
}

...
```

You can use this feature together with `browse` to read code in the
pager. Finally, if you want to read the code in your favourite editor,
just dump it to a file:

```bash
cpg.method.name("memcpy").callIn.dumpRaw |> "/tmp/foo.c"
```

We use `dumpRaw` here to skip syntax highlighting, as your editor will
most likely do that for you.

## Running Scripts

You can run scripts non-interactively using `joern`.

### From the outside

The `--script` option allows to execute scripts in a newly instantiated session.

For example,

```bash
./joern --script scripts/list-funcs.scala
```

executes the `list-funcs.scala` script included with Joern, which loads the CPG at `cpg.bin`, and writes out all function names.

You can also pass arguments to scripts. For example, the following script accepts the filename of a CPG (`cpgFile`) and a filename of a result file (`outFile`):

```scala
@main def exec(cpgFile: String, outFile: String) = {
   loadCpg(cpgFile)
   cpg.method.name.l |> outFile
}
```

It can be called as follows:

```bash
./joern --script test.sc --params cpgFile=/fullpath/to/cpg.bin,outFile=out.log
```

### During a running session with the ScriptManager

As an alternative to the `--script` option, during a running Joern session the ScriptManager provides some basic script handling.
It offers the following two methods:

```scala
def scripts(): List[ScriptDescription]
```

This will list all available scripts (`name`, `description`) that are stored in the `scripts/` folder of Joern.

```scala
def runScript(name: String, parameters: Map[String, String]): AnyRef
```

This will execute the script named `name` as already explained above. Any script parameters can be provided
using the map. Alternatively, a script may also be run on a cpg as follows:
```scala
val myCpg: Cpg = ...
myCpg.runScript("my-script")
```

### Extracting a PDG from a CPG
To extract a PDG for all methods in the CPG, you can run the following script included in your
Joern distribution:
```scala
cpg.runScript("general/pdg.sc")
```

If you are only interested in a subset of functions in the CPG, you may also provide a parameter
containing a regular expression, which will be used to filter out functions based on name:
```scala
cpg.runScript("general/pdg.sc", Map("methodRegex" -> ".*length.*"))
```

### Extracting a CFG from a CPG
As above, all Joern distribution also include scripts to extract a CFG from a CPG. To obtain a CFG for
all functions in a CPG, run the following script:
```scala
cpg.runScript("general/cfg-for-funcs.sc")
```

There is also a script which provides a CFG in the DOT format:
```scala
cpg.runScript("general/cfgToDot.sc")
```
