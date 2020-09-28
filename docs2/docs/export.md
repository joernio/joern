---
id: exporting
title: Exporting Graphs
---

Joern is used in academic research as a source for
intermediate graph representations of code, particularly in machine
learning and vulnerability discovery applications [e.g., [1,2,3,4,5](#references)]. To support
this use-case, Joern provides both plotting capabilities in the
interactive console as well as the `joern-export` command line
utility.

In summary, Joern can create the following graph representations for
C/C++ code:

* Abstract Syntax Trees (AST)
* Control Flow Graphs (CFG)
* Control Dependence Graphs (CDG)
* Data Dependence Graphs (DDG)
* Program Dependence graphs (PDG)
* Code Property Graphs
  ([CPG14](https://www.sec.cs.tu-bs.de/pubs/2014-ieeesp.pdf))


## The command line tool `joern-export`

All of these representations can be plotted and exported into the
graphviz dot format to enable processing with third party tools or via
external scripts.

To parse the code in `/src/directory` and dump Program Dependence
Graphs for all methods into the directory `outdir`, you can run the
following commands on the system shell:

```
joern-parse /src/directory
joern-export --repr pdg --out outdir
```

For a complete overview of options, run `joern-export --help`.


## Plotting and Exporting on the Joern Console

If you would like to explore graph representations interactively, you
can do so on the [Joern shell](/joern/shell). To this end, we define
the following steps on `method` nodes to dump representations in dot
format.

```
cpg.method($name).dotAst.l // output AST in dot format
cpg.method($name).dotCfg.l // output CFG in dot format
...
cpg.method($name).dotCpg14.l // output CPG'14 in dot format
```

You can also plot and view representations using the following
queries:

```
cpg.method($name).plotDotAst // plot AST
cpg.method($name).ploDotCfg // plot CFG
...
cpg.method($name).plotDotCpg14 // plot CPG'14
```

Note that the `ossdataflow` layer needs to have been calculated for
the source CPG via `run.ossdataflow`.

### Example

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

## Dumping representations for all functions from the shell

You can also dump all representations into the directory `out` using

```
run.dumpast
run.dumpcfg
...
run.dumpcpg14
```

## References

Research that employs Joern as an extraction tool for intermediate
representations of code:

(1) [Devign: Effective Vulnerability Identification by Learning
Comprehensive Program Semantics via Graph Neural Networks, Zhou et al.,
NIPS'19](https://papers.nips.cc/paper/9209-devign-effective-vulnerability-identification-by-learning-comprehensive-program-semantics-via-graph-neural-networks.pdf)

(2) [Source Code Authorship Attribution using LongShort-Term Memory
Based Networks, Alsulami et al., ESORICS'17](https://www.cs.drexel.edu/~greenie/stylometry-esorics.pdf)

(3)[VulPecker: an automated vulnerability detection system based on
code similarity analysis, Li et al.,
ACSAC'16](http://www.cs.utsa.edu/~shxu/socs/VulPecker.pdf)

(4) [Git blame who?: Stylistic authorship attribution of small,
incomplete source code fragments, Dauber et al.,
PETS-19/3](https://content.sciendo.com/view/journals/popets/2019/3/article-p389.xml?language=en)

(5) [SPIDER: Enabling Fast Patch Propagation In Related Software
Repositories, Machiry et al., S&P'20](https://machiry.github.io/files/spider.pdf)
