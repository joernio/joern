Ready-to-run scripts for Joern
===============================

This is our collection of ready-to-run scripts for `joern`.
You can start them by using one of the following methods:

```scala
def runScript(name: String, params: Map[String, String]): AnyRef
def runScript(name: String, params: Map[String, String], cpg: Cpg): AnyRef
def runScript(name: String, params: Map[String, String], cpgFilename: String): AnyRef
```

inside a running Joern session. You can also use the `runScript` method available on the `cpg`:
```scala
cpg.runScript("my-script.sc", Map("param" -> "value"))
```

To get a list of available scripts you can call:

```scala
def scripts(): List[ScriptDescription]
```

which returns the name and a short description for all scripts located in the folder `scripts/`.

Please send a PR if you have a nice script to share :)
