Ready-to-run scripts for Joern
===============================

This is our collection of ready-to-run scripts for `joern`.
You can start them by using one of the following methods:

```scala
def runScript(name: String): AnyRef
def runScript(name: String, cpg: Cpg): AnyRef
def runScript(name: String, cpgFilename: String): AnyRef
```

inside a running Joern session.

To get a list of available scripts you might want to call:

```scala
def scripts(): List[ScriptDescription]
```

which returns the name and a short description for all scripts located in the folder `scripts/`.

Please send a PR if you have a nice script to share :)
