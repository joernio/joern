# Joern: an open-source code analysis platform for C/C++

## Building

To build joern, please install the following: 

* Python3
  - Link: https://www.python.org/downloads/
* Java runtime 8
  - Link: http://openjdk.java.net/install/
* Scala build tool (sbt)
  - Link: https://www.scala-sbt.org/
  
With those dependencies installed, run `./build.sh`, which will
build the code property graph generator for C/C++ and a querying component.

## CPG Creation

The CPG is an open and language agnostic
[format](https://github.com/ShiftLeftSecurity/codepropertygraph#base-schema-for-the-code-property-graph).
You can either use your own CPG generators or use our
open source C/C++ frontend [fuzzyc2cpg](https://github.com/ShiftLeftSecurity/fuzzyc2cpg)
to create a CPG for any C/C++ program.

Run `./fuzzyc2cpg.sh <path/to/directory> --out <path/to/cpg/cpg_name>` in order generate a CPG (filename must be appended). If you leave the ```--out``` the CPG is generated in the local folder.

## CPG Querying

Run `./joern.sh <path/to/cpg>` to query the CPG.
By default Joern only queries the CPG for a all methods defined in the
CPG but you can run your own queries by modifing
[src/main/scala/io/shiftleft/Main.scala], rebuilding and executing Joern again.

## Warm Up

Let's warm up a bit with a small example that violates the coding standard.

```c
#include <stdlib.h>
struct node {
    int value;
    struct node *next;
};
void free_list(struct node *head) {
    for (struct node *p = head; p != NULL; p = p->next) {
        free(p);
    }
}
```

The small snippet is a clasic example from Kernighan and Ritchie [Kernighan 1988]. 
Note that the flaw is intentionally incorporated in their book ;)

The problem here is that ```p```
is freed before ```p->next``` is executed. 
The snipped can be found under `./tests/free`

We fire up our frontend to build the CPG:

`./fuzzyc2cpg.sh tests/free `

This gives us a ```cpg.bin.zip``` in our local folder.
Now fire up `./joern.sh cpg.bin.zip`. Well, this doens't give us much right? 

What you should see are all the methods in a rather low level representation.

```
...

Method(Some(v[258]),<operator>.assignment,<operator>.assignment,TODO assignment signature,NAMESPACE_BLOCK,<global>,None,None,None,None)

...
```

We can lighten this up a little bit by the beauty of our Scala like DSL.  

Open [src/main/scala/io/shiftleft/Main.scala], 

your code should look like this:

```scala
package io.shiftleft

import io.shiftleft.cpgloading.tinkergraph.CpgLoader
import io.shiftleft.queryprimitives.steps.Implicits._

object Main extends App {
  val cpg = CpgLoader.loadCodePropertyGraph(args(0), runEnhancements = true)

  // Print all methods starting with "<operator>"
  cpg.method.p
}
```


Now add the following lines to after `cpg.method.p`:

```scala
println("------ METHODS -----")
cpg.method.l.foreach(m => println(m.fullName))
```

The `.l.foreach` construct returns a List collection on which we can directly operate with `foreach`. This construct runs over each
`Method` Object of our CPG and prints the *fullname* of it.

Now all you need to do is:

`sbt stage` in the local `joern` folder. This will build our new script for Joern to process.

Now fire up  `./joern.sh cpg.bin.zip` and you will see the new output:

```
------ METHODS -----
<operator>.assignment
free_list
<operator>.notEquals
<operator>.indirectMemberAccess
free
```
Obviously, there is some information we don't want to see.
Let's filter it to get only those methods that start with `free`.

Open up your *Main.scala* file and the followng lines:

```scala
  println("----- Filtered -----")
  cpg.method.name("free.*").l.foreach(m => println(m.fullName)
```

Againg do:

1. *sbt stage*
2. *./joern.sh cpg.bin.zip*

This should give us the desired output.

<h1> Time to see some flows </h1>

Our example doesn't give us much space for serious flows. We'll look at some more complex cases later. But for now, let's use our  it.

Open up your *Main.scala* file and add:

```scala
  val source = cpg.identifier
  val sink = cpg.call.name("free")
  println(sink.reachableByFlows(source).p)
```

Here we define the source to be an **identifier**; identifiers. Think of it as an unique object that represents a variable at that specific program point where it is used or defined.

A thorough description on the concept of the CPG schema and its query primitives can be found at:

- https://github.com/ShiftLeftSecurity/codepropertygraph
- https://ocular.shiftleft.io/api/io/shiftleft/queryprimitives/index.html


With  ```cpg.call.name("free")```  we query for
all `free` methods that are actually called. Think of a **CALL** objects as a unique representative for each specific call to *free*. Apparantly we would get more call objects for our query if there were more calls to free.

Add this to your *Main.scala*:

```scala
 println("----- Call to free ----")
 cpg.call.name("free").l.foreach(call => println(call.code))
```

This should give the us the following output:

```
----- Call to free ----
free(p)
```

Don't forget to `sbt stage` before you run joern on the cpg.
For a faster compile process you can run *sbt* in your local folder. This will open a
sbt shell. For each change you make in your *Main.scala* file you can now run `stage` in the sbt shell. This will speed up the process of experimenting with joern.

Back to our flow problem:

running ```sink.reachableByFlows(source)``` will basically traverse the flow of data dependency back to each source that we provided. In this case all *identifiers*.

If we run this script we get the following output:

```
 _____________________________________________________
 | tracked  | lineNumber| method   | file             |
 |====================================================|
 | *p = head| 7         | free_list| tests/free/free.c|
 | free(p)  | 8         | free_list| tests/free/free.c|

 _______________________________________________________
 | tracked    | lineNumber| method   | file             |
 |======================================================|
 | *p = head  | 7         | free_list| tests/free/free.c|
 | p->next    | 7         | free_list| tests/free/free.c|
 | p = p->next| 7         | free_list| tests/free/free.c|
 | free(p)    | 8         | free_list| tests/free/free.c|

 _______________________________________________________
 | tracked    | lineNumber| method   | file             |
 |======================================================|
 | p->next    | 7         | free_list| tests/free/free.c|
 | p = p->next| 7         | free_list| tests/free/free.c|
 | free(p)    | 8         | free_list| tests/free/free.c|

 _______________________________________________________
 | tracked    | lineNumber| method   | file             |
 |======================================================|
 | p = p->next| 7         | free_list| tests/free/free.c|
 | free(p)    | 8         | free_list| tests/free/free.c|

 ___________________________________________________
 | tracked| lineNumber| method   | file             |
 |==================================================|
 | free(p)| 8         | free_list| tests/free/free.c|
```

This outputs shows you each stage during the traversal starting from `free(p)` itself.

What about the `p != NULL`?  Let's check this with:

```scala
val source2 = cpg.identifier
val sink2 = cpg.method.name("free_list").callOut.code("p != NULL")
println(sink2.reachableByFlows(source2).p))
```

Here we explicitly filter for the our `p != NULL` case in the `free_list` function.


```
 _____________________________________________________
 | tracked  | lineNumber| method   | file             |
 |====================================================|
 | *p = head| 7         | free_list| tests/free/free.c|
 | p != NULL| 7         | free_list| tests/free/free.c|

 _______________________________________________________
 | tracked    | lineNumber| method   | file             |
 |======================================================|
 | *p = head  | 7         | free_list| tests/free/free.c|
 | p->next    | 7         | free_list| tests/free/free.c|
 | p = p->next| 7         | free_list| tests/free/free.c|
 | p != NULL  | 7         | free_list| tests/free/free.c|

 _______________________________________________________
 | tracked    | lineNumber| method   | file             |
 |======================================================|
 | p->next    | 7         | free_list| tests/free/free.c|
 | p = p->next| 7         | free_list| tests/free/free.c|
 | p != NULL  | 7         | free_list| tests/free/free.c|

 _______________________________________________________
 | tracked    | lineNumber| method   | file             |
 |======================================================|
 | p = p->next| 7         | free_list| tests/free/free.c|
 | p != NULL  | 7         | free_list| tests/free/free.c|

 _____________________________________________________
 | tracked  | lineNumber| method   | file             |
 |====================================================|
 | p != NULL| 7         | free_list| tests/free/free.c|

```

It shows us the flow of p into the sanitizer of the for loop.


