+++
title="Joern: open-source query-based code analysis for C/C++"
+++

<img src="/docs/images/logo_design_v01_R01.png" style="background-color:black;">


Joern is a platform for robust analysis of C/C++ code. It generates
semantic code property graphs, a graph representation of code for
cross-language code analysis. Semantic code property graphs are stored
in a custom graph database. This allows code to be mined using
search queries formulated in a domain-specific query language based on
the graph traversal language Gremlin. Joern provides a peek into the underlying technology that powers the commercial code analyzer [Ocular](https://ocular.shiftleft.io/).

The core features of Joern are:

* **Fuzzy Parsing of C/C++.** Joern employs a fuzzy parser for C/C++ based on the concept of Island grammars. The parser enables importing arbitrary code even if a working build environment cannot be supplied or parts of the code are missing.
* **Semantic Code Property Graphs.** Joern creates semantic code property graphs from the fuzzy parser output and stores them in an in-memory graph database. SCPGs are a language-agnostic intermediate representation of code designed for query-based code analysis. For background information on code property graphs, we strongly encourage you to read the original paper on the topic (https://fabs.codeminers.org/papers/2014-ieeesp.pdf) and the specification of the semantic CPG at https://github.com/ShiftLeftSecurity/codepropertygraph .
* **Intelligent Search Queries.** Joern offers a stronly-typed Scala-based extensible query language for code analysis based on Gremlin-Scala. This language can be used to manually formulate search queries for vulnerabilities as well as automatically infer them using machine learning techniques.
* **Extendable via CPG passes.** Semantic code property graphs are multi-layered, offering information about code on different levels of abstraction. Joern comes with many default passes, but also allows users to add passes to include additional information in the graph, and extend the query language accordingly.

