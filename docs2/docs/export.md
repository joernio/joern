---
id: exporting
title: Exporting Graphs
---

Joern can be used to generate the following intra-procedural
intermediate graph representations of code (among others):

* Abstract Syntax Trees (AST)
* Control Flow Graphs (CFG)
* Control Dependence Graphs (CDG)
* Data Dependence Graphs (DDG)
* Program Dependence graphs (PDG)
* Code Property Graphs (CPG14)

:::note
The Program Dependence Graph (PDG) of a method corresponds to the
graph obtained by merging its control dependence graph and its data
dependence graph.
:::


All of these representations can be plotted and exported into the
graphviz dot format to enable processing with third party tools or via
external scripts.

## Example:

Generate the CPG along with the data flow layer for a sample function
named `myfunc`.


```
joern> importCode.c.fromString( """
           int myfunc(int b) {
             int a = 42;
             if (b > 10) {
                foo(a)
             }
             bar(a);
           }
           """
       ) 

joern> run.ossdataflow
```

You can now plot the AST as follows:

```
joern> cpg.method("myfunc").plotDotAst 
```

You can obtain the dot representation of the AST as well:

```
joern> cpg.method("myfunc").dotAst.l
res4: List[String] = List(
  """digraph myfunc {  
"1000102" [label = "(METHOD,myfunc)" ]
"1000103" [label = "(PARAM,int b)" ]
"1000104" [label = "(BLOCK,,)" ]
"1000105" [label = "(LOCAL,a: int)" ]
"1000106" [label = "(<operator>.assignment,a = 42)" ]
"1000107" [label = "(IDENTIFIER,a,a = 42)" ]
"1000108" [label = "(LITERAL,42,a = 42)" ]
"1000109" [label = "(CONTROL_STRUCTURE,if (b > 10),if (b > 10))" ]
"1000110" [label = "(<operator>.greaterThan,b > 10)" ]
"1000111" [label = "(IDENTIFIER,b,b > 10)" ]
"1000112" [label = "(LITERAL,10,b > 10)" ]
"1000113" [label = "(BLOCK,,)" ]
"1000114" [label = "(bar,bar(a))" ]
"1000115" [label = "(IDENTIFIER,a,bar(a))" ]
"1000116" [label = "(METHOD_RETURN,int)" ]
  "1000102" -> "1000103"  
  "1000102" -> "1000104"  
  "1000102" -> "1000116"  
  "1000104" -> "1000105"  
  "1000104" -> "1000106"  
  "1000104" -> "1000109"  
  "1000104" -> "1000114"  
  "1000106" -> "1000107"  
  "1000106" -> "1000108"  
  "1000109" -> "1000110"  
  "1000109" -> "1000113"  
  "1000110" -> "1000111"  
  "1000110" -> "1000112"  
  "1000114" -> "1000115"  
}
"""
)
```

You can also dump all ASTs of all functions into the directory `out`:

```
run.dumpast
```

Corresponding utility methods exist for all other representations,
e.g.,


```
run.dumpddg
```

will dump the data dependence graph.
