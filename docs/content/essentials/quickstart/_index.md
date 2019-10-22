+++
title="Quick Start"
weight=1
+++

Clone and build the Joern:

```
git clone https://github.com/ShiftLeftSecurity/joern.git
cd joern
sbt createDistribution
mkdir -p install
cd install
unzip ../joern-cli.zip
unzip ../joern-server.zip
cd joern-cli
```

Create a cpg (stored in `cpg.bin.zip`):

```
./joern-parse joern-cli/src/test/resources/testcode/free
```

Execute the joern shell and explore it

```bash
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

