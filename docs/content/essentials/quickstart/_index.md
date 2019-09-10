+++
title="Quick Start"
weight=1
+++

Clone and build the Joern:

```
git clone https://github.com/ShiftLeftSecurity/joern.git
cd joern
sbt stage
```

Create a cpg (stored in `cpg.bin.zip`):

```
./joern-parse joern-cli/src/test/resources/testcode/fre
```

Execute the joern shell and explore it

```
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

joern> loadCpg("cpg.bin.zip")

joern> cpg.method.l 


res2: List[io.shiftleft.codepropertygraph.generated.nodes.Method] = List(
  Method(
    64L,
    "<operator>.indirectMemberAccess",
    "<operator>.indirectMemberAccess",
    true,
    "TODO assignment signature",
    "NAMESPACE_BLOCK",
    "<global>",
    None,
    None,
    0,
    None
  ),
  Method(61L, "free", "free", true, "TODO assignment signature", "NAMESPACE_BLOCK", "<global>", None, None, 0, None),
  Method(58L, "<operator>.notEquals", "<operator>.notEquals", true, "TODO assignment signature", "NAMESPACE_BLOCK", "<global>", None, None, 0, None),
  Method(56L, "<operator>.assignment", "<operator>.assignment", true, "TODO assignment signature", "NAMESPACE_BLOCK", "<global>", None, None, 0, None),
  Method(11L, "free_list", "free_list", false, "void(struct node *)", null, null, Some(7), Some(0), null, None)

joern> Ctrl+d
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
