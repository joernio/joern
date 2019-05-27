+++
title="Interfacing with Joern"
weight=4
+++

Joern is both an interactive code analysis tool and a library to build custom static analyzers. To achieve this, it offers two different interfaces.

- **Java library.** Joern/Ocular offer a Java API that can be used from JVM-based languages such as Java, Scala, Groovy, Jython, or Kotlin. This interface is best if you are looking to build a standalone analyzer based on Joern and need control over graph loading and task scheduling.

- **REST API.** For interactive querying and compatibility with non-JVM programming languages, Joern/Ocular offer a REST-based server. This is the right interface for integration of Joern with other services using small scripts, interactive shell-based querying of graphs, and for UIs.

