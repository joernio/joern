+++
title="Querying the graph"
weight=3
+++

One you have [created a code property graph](/docs/creation) stored at
`path/to/cpg`, you can load and query it as follows:

```
./joern.sh <path/to/cpg>
```

This will run the script in
```
src/main/scala/io/shiftleft/joern/Main.scala
```
which, by default, queries the CPG for all methods. You can modify this script to run other queries. In the following, we will go through a few examples to get you acquainted with joern.

### Running your first query ("hello-cpg")

We begin with a small example of a use-after-free condition. Consider the snippet below

{{<snippet file="src/test/resources/testcode/free/free.c" language="c">}}

which you can find under `src/test/resources/testcode/free`. We create a CPG for this code as follows:

`./fuzzyc2cpg.sh src/test/resources/testcode/free/ `

This produces a ```cpg.bin.zip``` in our local folder.
Now launch
```
./joern.sh cpg.bin.zip
```
which gives you all methods in a rather raw representation:

```
...
Method(Some(v[258]),<operator>.assignment,<operator>.assignment,TODO assignment signature,NAMESPACE_BLOCK,<global>,None,None,None,None)
...
```
### Modifying queries

Next, let's edit `src/main/scala/io/shiftleft/joern/Main.scala` to run different queries,

You should see the following content before editing:
{{<snippet file="src/main/scala/io/shiftleft/joern/Main.scala" language="scala">}}

Now add the following lines after `cpg.method.p`:

```scala
println("------ METHODS -----")
cpg.method.l.foreach(m => println(m.fullName))
```

The `.l.foreach` construct returns a List collection on which we can
directly operate with `foreach`. This construct runs over each
`Method` Object of our CPG and prints the *full name* of it.

We recompile `Main.scala` by issuing the command
```
sbt stage
```
in joern's top-level directory. We can then run the newly created program via 
```
./joern.sh cpg.bin.zip
```
which will produce the following output:

```
------ METHODS -----
free
free_list
<operator>.assignment
<operator>.notEquals
<operator>.indirectMemberAccess
```
Let's filter only for methods that start with `free`.

Open up the *Main.scala* file again and add the following lines:

```scala
  println("----- Filtered -----")
  cpg.method.name("free.*").l.foreach(m => println(m.fullName)
```
Again do:

```
sbt stage
./joern.sh cpg.bin.zip
```

This should give us the following output:

```
----- Filtered -----
free
free_list
```

## Querying for locals

Let's add another function to our snippet.

```c
int flow(int p0) {
  int a = p0;
  int b=a;
  int c=0x31;
  int z = b + c;
  z++;
  int x = z;
  return x;
  }
```

We'll look at the flows later. For now we are interested in the locals, i.e. local variables.



Add this to your *Main.scala*:

```scala
  println("----- Locals -----")
  val locals = cpg.local.l
  println(locals.foreach(local => println(local.name)))
```

Here we collect all local variables over all functions into a list (*locals*). These locals are represented by an object, so we print out each name. We should get the following lines at the end of the output:

```
----- Locals -----
a
b
c
z
x
q
p
```

The queries we defined so far are a little inconvenient. In fact, it is not the Joern way to do it.
A reformulation of the query above which conforms more to the Joern way looks like this:

```scala
cpg.local.name.p
```

Much better isn't it? ;)

It will give us the same output. The `p` suffix prints out the name of each local variable.

This also works for our filter we defined earlier:

```scala
 println("----- Filtered -----")
 cpg.method.name("free.*").name.p
```

At the beginning of this section we queried for all functions and printed them; we can reformulate it as well to conform more to the Joern way:

```scala
println("------ METHODS -----")
cpg.method.name.p
```

Joern allows us to filter locals function wise:

```scala
  println("----- local vars flow -----")
  cpg.method.name("flow").local.name.p

  println("----- local vars free_list -----")
  cpg.method.name("free_list").local.name.p
```

Note that we just added `.name(regex)` to our `method` query.

We can also filter for variables we are interested in:



```scala
cpg.method.local.name("a*").name.p
```

will give us variables which start with `a`.

Want to know where it comes from?

```scala
cpg.local.name("a*").file.name.p
```

Let's add some type information to our query:

```scala
cpg.local.map(l => (l.name, l.typeFullName)).p
```
This adds the following lines to our output:

```
(a,int)
(b,int)
(c,int)
(z,int)
(x,int)
(q,struct node *)
(p,struct node *)
```

Function signatures:

```scala
cpg.method.signature(".*struct.*").name.p
```

This query gives us all functions that have a `struct` in their signature which in our case is `free_list`.

## Time to see some flows

Our example doesn't give us much space for serious flows. We'll look at some more complex cases later. But for now, let's use our  it.

Open up your *Main.scala* file and add:

```scala
  val source = cpg.identifier
  val sink = cpg.call.name("free")
  println(sink.reachableByFlows(source).p)
```
Here we define the source to be the set of  **identifiers**. Think of an identifier a an unique object that represents a variable at that specific program point where it is used or defined.

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

We introduced the query to go into call a little bit.
However, a better way to formulate our sink is by its arguments it receives.
Replace our `val sink` with:

```scala
val sink = cpg.method("free").parameter.argument
```

It will give us the same output, but is more consistent in what we want to achieve, i.e., track the data dependency of the arguments.

Don't forget to `sbt stage` before you run joern on the cpg.
For a faster compile process you can run *sbt* in your local folder. This will open a sbt shell. For each change you make in your *Main.scala* file you can now run `stage` in the sbt shell. This will speed up the process of experimenting with joern.

Back to our flow problem:

running ```sink.reachableByFlows(source)``` will basically traverse the flow of data dependency back to each source that we provided. In this case all *identifiers*.

If we run this script we get the following output:

```
 ________________________________________________________
 | tracked  | lineNumber| method   | file              |
 |=====================================================|
 | *p = head| 19        | free_list| tests/free2/free.c|
 | free(p)  | 21        | free_list| tests/free2/free.c|

 ________________________________________________________
 | tracked    | lineNumber| method   | file              |
 |=======================================================|
 | *p = head  | 19        | free_list| tests/free2/free.c|
 | p->next    | 20        | free_list| tests/free2/free.c|
 | q = p->next| 20        | free_list| tests/free2/free.c|
 | p = q      | 19        | free_list| tests/free2/free.c|
 | free(p)    | 21        | free_list| tests/free2/free.c|

 ________________________________________________________
 | tracked    | lineNumber| method   | file              |
 |=======================================================|
 | p->next    | 20        | free_list| tests/free2/free.c|
 | q = p->next| 20        | free_list| tests/free2/free.c|
 | p = q      | 19        | free_list| tests/free2/free.c|
 | free(p)    | 21        | free_list| tests/free2/free.c|

 ________________________________________________________
 | tracked    | lineNumber| method   | file              |
 |=======================================================|
 | q = p->next| 20        | free_list| tests/free2/free.c|
 | p = q      | 19        | free_list| tests/free2/free.c|
 | free(p)    | 21        | free_list| tests/free2/free.c|

 ____________________________________________________
 | tracked| lineNumber| method   | file              |
 |===================================================|
 | p = q  | 19        | free_list| tests/free2/free.c|
 | free(p)| 21        | free_list| tests/free2/free.c|

 ____________________________________________________
 | tracked| lineNumber| method   | file              |
 |===================================================|
 | free(p)| 21        | free_list| tests/free2/free.c|

```

This outputs shows you each stage during the traversal starting from `free(p)` itself.

What about the `p != NULL`?  Let's check this with:

```scala
val source2 = cpg.identifier
val sink2 = cpg.method.name("free_list").callOut.code("p != NULL")
println(sink2.reachableByFlows(source2).p))
```
Here we explicitly filter for the `p != NULL` case in the `free_list` function.


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

It shows us the flow of `p` into the sanitizer of the for loop.


Let's have a look at our `flow` function. Say, we want to track the flow from each return value to each data dependent identifier.
A query can look like follows:

```scala
 val source3 = cpg.identifier
 val sink3 = cpg.method.name("flow").methodReturn
 println(sink3.reachableByFlows(source3).p)
```

methodReturn gives us the formal return value of the function, i.e.,
each return value is connected to the object we obtain through methodReturn.


Our output:

```
 ___________________________________________________
 | tracked  | lineNumber| method| file              |
 |==================================================|
 | a = par  | 8         | flow  | tests/free2/free.c|
 | b=a      | 9         | flow  | tests/free2/free.c|
 | b + c    | 11        | flow  | tests/free2/free.c|
 | z = b + c| 11        | flow  | tests/free2/free.c|
 | x = z    | 13        | flow  | tests/free2/free.c|
 | return x;| 14        | flow  | tests/free2/free.c|
 | RET      | 7         | flow  | tests/free2/free.c|

 ___________________________________________________
 | tracked  | lineNumber| method| file              |
 |==================================================|
 | b=a      | 9         | flow  | tests/free2/free.c|
 | b + c    | 11        | flow  | tests/free2/free.c|
 | z = b + c| 11        | flow  | tests/free2/free.c|
 | x = z    | 13        | flow  | tests/free2/free.c|
 | return x;| 14        | flow  | tests/free2/free.c|
 | RET      | 7         | flow  | tests/free2/free.c|

 ___________________________________________________
 | tracked  | lineNumber| method| file              |
 |==================================================|
 | c=0x31   | 10        | flow  | tests/free2/free.c|
 | b + c    | 11        | flow  | tests/free2/free.c|
 | z = b + c| 11        | flow  | tests/free2/free.c|
 | x = z    | 13        | flow  | tests/free2/free.c|
 | return x;| 14        | flow  | tests/free2/free.c|
 | RET      | 7         | flow  | tests/free2/free.c|

 ___________________________________________________
 | tracked  | lineNumber| method| file              |
 |==================================================|
 | b + c    | 11        | flow  | tests/free2/free.c|
 | z = b + c| 11        | flow  | tests/free2/free.c|
 | x = z    | 13        | flow  | tests/free2/free.c|
 | return x;| 14        | flow  | tests/free2/free.c|
 | RET      | 7         | flow  | tests/free2/free.c|

 ___________________________________________________
 | tracked  | lineNumber| method| file              |
 |==================================================|
 | z = b + c| 11        | flow  | tests/free2/free.c|
 | x = z    | 13        | flow  | tests/free2/free.c|
 | return x;| 14        | flow  | tests/free2/free.c|
 | RET      | 7         | flow  | tests/free2/free.c|

 ___________________________________________________
 | tracked  | lineNumber| method| file              |
 |==================================================|
 | x = z    | 13        | flow  | tests/free2/free.c|
 | return x;| 14        | flow  | tests/free2/free.c|
 | RET      | 7         | flow  | tests/free2/free.c|

 ___________________________________________________
 | tracked  | lineNumber| method| file              |
 |==================================================|
 | return x;| 14        | flow  | tests/free2/free.c|
 | RET      | 7         | flow  | tests/free2/free.c|

```

Again the outputs show you each stage during the traversal.
Since we defined the source to be all identifiers, the engine outputs each hit to
an indentifier it finds along its paths backwards starting from the sinks.

We can restrict the flows to stop at each `z` the engine encounters:

```scala
 val source4 = cpg.identifier.name("z")
 val sink4 = cpg.method.name("flow").methodReturn
 println(sink4.reachableByFlows(source4).p)
```

produces the following output:

```
 ___________________________________________________
 | tracked  | lineNumber| method| file              |
 |==================================================|
 | z = b + c| 11        | flow  | tests/free2/free.c|
 | x = z    | 13        | flow  | tests/free2/free.c|
 | return x;| 14        | flow  | tests/free2/free.c|
 | RET      | 7         | flow  | tests/free2/free.c|

 ___________________________________________________
 | tracked  | lineNumber| method| file              |
 |==================================================|
 | x = z    | 13        | flow  | tests/free2/free.c|
 | return x;| 14        | flow  | tests/free2/free.c|
 | RET      | 7         | flow  | tests/free2/free.c|

```
