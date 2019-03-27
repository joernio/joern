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

Let's warm up a bit with a small and simple example:

```c
#include <stdlib.h>
struct node {
    int value;
    struct node *next;
};

void free_list(struct node *head) { 
  struct node *q;
  for (struct node *p = head; p != NULL; p = q) { 
    q = p->next;
    free(p);
    } 
}
```

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

We can shed light on this using our beautiful Scala-based DSL for code analysis.

Open [src/main/scala/io/shiftleft/Main.scala], 

your code should look like this:

```scala
package io.shiftleft

import io.shiftleft.cpgloading.tinkergraph.CpgLoader
import io.shiftleft.queryprimitives.steps.Implicits._
import io.shiftleft.passes.dataflows._

object Main extends App {
  val cpg = CpgLoader.loadCodePropertyGraph(args(0), runEnhancements = true)

  // Print all methods
  cpg.method.p
}
```

Now add the following lines after `cpg.method.p`:

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
free
free_list
<operator>.assignment
<operator>.notEquals
<operator>.indirectMemberAccess
```
Obviously, there is some information we don't want to see.
Let's filter it to get only those functions that start with `free`.

Open up your *Main.scala* file and the followng lines:

```scala
  println("----- Filtered -----")
  cpg.method.name("free.*").l.foreach(m => println(m.fullName)
```
Againg do:

1. *sbt stage*
2. *./joern.sh cpg.bin.zip*

This should give us the desired output:

```
----- Filtered -----
free
free_list
```

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

The queries we defined so far are a little inconvenient.
In fact, it is not the Joern way to do it.
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

<h1> Time to see some flows </h1>

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


# CVE-2016-6480 

### Description
- Race condition in the ioctl_send_fib function in drivers/scsi/aacraid/commctrl.c


Here we show how Joern helps you in the process of code auditing. The CVE bug concerns version 4.7 of the Linux Kernel.

### Setup
```bash
$ git clone https://github.com/torvalds/linux`
$ cd linux
$ git checkout v4.7
```

This setup give us access to the vulnerable driver.

Build the CPG for the driver:

`./fuzzyc2cpg.sh path/to/kernel/linux/drivers/scsi/aacraid`


### Analysis

The first thing which comes to mind is the interaction from user to kernel space. This is usually done with `copy_from_user`. Let's see if any data from user space to kernel space is copied:

```scala
 cpg.call.name("copy_from_user").code.p
```

We should get the following output:

```
copy_from_user(&qd, arg, sizeof (struct aac_query_disk))
copy_from_user(&dd, arg, sizeof (struct aac_delete_disk))
copy_from_user(&dd, arg, sizeof (struct aac_delete_disk))
copy_from_user((void *)kfib, arg, sizeof(struct aac_fibhdr))
copy_from_user(kfib, arg, size)
copy_from_user((void *)&f, arg, sizeof(struct fib_ioctl))
copy_from_user(&fibsize, &user_srb->count,sizeof(u32))
copy_from_user(user_srbcmd, user_srb,fibsize)
copy_from_user(p,sg_user[i],upsg->sg[i].count)
copy_from_user(p,sg_user[i],upsg->sg[i].count)
copy_from_user(p,sg_user[i],usg->sg[i].count)
copy_from_user(p, sg_user[i],\n\t\t\t\t\t\t\tupsg->sg[i].count)
```

This doesn't look so bad.

Get some flows from `copy_from_user`:

```scala
val sinkArguments = pg.method.name("copy_from_user").parameter.argument
println(sinkArguments.reachableByFlows(cpg.identifier).p)
```

Ok, this gives us a lot of information back. The following query gives us the 
number.

```scala
println(sinkArguments.reachableByFlows(cpg.identifier).l.size)
```

should be 302. 

Let's look for something which doesn't overwhelm us.
It would be interesting to have an estimate if the arguments of `copy_from_user`
are sanitized.

Since we don't have direct definitions at if expressions, we do not get any reaching definition information out of them. But we get information that flows into if expressions.

Maybe we can work our way around this. Fire up your *Main.scala* and add the following lines:

```scala
val reachingDefs1 = cpg.method
                       .name("copy_from_user")
                       .parameter
                       .argument
                       .reachableBy(cpg.identifier)
                       .toSet
```

So far we have used `reachableByFlows` to construct and print out our flows.
Sometimes we do not want that much of a detail. With `reachableBy` we tell the engine that we are not interested in the details of the data flow paths, but rather want to know which of our *sources* are hit. At the end of the query we collect the sources that are hit into a set.

```scala
val reachingDefs2 = cpg.method
                       .name(".*less.*", ".*greater.*")
                       .parameter
                       .argument
                       .reachableBy(cpg.identifier)
                       .toSet
```

Let's take a break and dismantle this query:

-  We restrict the flows running to expressions that involve the *less* **or** *greater* keyword. Note that internally each binary operation (+,-,>,< etc.) is also treated as a function. So we look for its arguments.
-  Data dependency is tracked back to each identifier it hits and collected into a set.
  
Now we can check if we have an intersection between these two sets which
gives us an estimate on what arguments of `copy_from_user` might be sanitized.


```scala
reachingDefs1.intersect(reachingDefs2).foreach(elem => println(elem.code))
```

should give us:

```
kmalloc(fibsize, GFP_KERNEL)
kmalloc(actual_fibsize - sizeof(struct aac_srb)\n\t\t\t  + sizeof(struct sgmap), GFP_KERNEL)
user_srbcmd->sg
kfib->header
size = le16_to_cpu(kfib->header.Size) + sizeof(struct aac_fibhdr)
actual_fibsize = sizeof(struct aac_srb) - sizeof(struct sgentry) +\n\t\t((user_srbcmd->sg.count & 0xff) * sizeof(struct sgentry))
* upsg = (struct user_sgmap64*)&user_srbcmd->sg
i = 0
user_srbcmd->sg
usg = kmalloc(actual_fibsize - sizeof(struct aac_srb)\n\t\t\t  + sizeof(struct sgmap), GFP_KERNEL)
user_srbcmd->sg
* upsg = &user_srbcmd->sg
user_srbcmd = kmalloc(fibsize, GFP_KERNEL)
i = 0
fibsize = 0
aac_fib_alloc(dev)
user_srbcmd->sg.count
kfib = fibptr->hw_fib_va
kfib->header.Size
actual_fibsize - sizeof(struct aac_srb)\n\t\t\t  + sizeof(struct sgmap)
fibptr = aac_fib_alloc(dev)
i = 0
fibptr->hw_fib_va
i = 0

```

This is actually quite nice; we can see that most *potential* checks involve some kind of a *size* element (as we might expect). 

At the beginning of this section we saw some `copy_from_user` outputs.
Let's look at those that have `kfib` as their first argument. This decision is not made randomly. If we look at the output above we see that `kfib` is an interesting pointer which gives us access to an header and its size seems to have an involvement in a check: 
`kfib->header.Size`.

We can cofirm this in the source code (`commctrl.c:90)`:

```c
size = le16_to_cpu(kfib->header.Size) + sizeof(struct aac_fibhdr);
if (size < le16_to_cpu(kfib->header.SenderSize))
                      ........
```

We use the following query to filter for `copy_from_user` looking for `kfib` as an argument:

```scala 
cpg.call.name("copy_from_user").code(".*kfib.*").l
```
we could also do:

```scala
 cpg.call.name("copy_from_user").filter(call => call.argument.code(".*kfib.*")).l
```

Let's print them out:

```
cpg.call.name("copy_from_user")
.filter(call => call.argument.code(".*kfib.*"))
.l
.foreach(call => println(call.code))
```

Output:

```
copy_from_user((void *)kfib, arg, sizeof(struct aac_fibhdr))
copy_from_user(kfib, arg, size)
```

Nice, we have two of them. If we find flows from these sinks to a common ancestor which defines `kfib` and there is no other definition of `kfib` along our way we might have a double fetch.

```scala
 val cfu1 = cpg.call.name("copy_from_user")
   .code(".*kfib.*")
   .l
   .head
   .start
   .reachableBy(cpg.identifier)
   .toSet

  val cfu2 = cpg.call.name("copy_from_user")
    .code(".*kfib.*")
    .l
    .last
    .start
    .reachableBy(cpg.identifier)
    .toSet
    .intersect(cfu1)


   cfu2.foreach(elem => println(elem.code, elem.lineNumber.get))
```

This is a similar pattern as we did above with our reaching definitions to sanitizers.
You might wonder what the `start` does here. It basically tells our engine to start a 
fresh traversal *starting* at the given node. In this case we filtered with *head* and
*last* to get those nodes. Play around with this to grasp the idea.

Our output:

```
(aac_fib_alloc(dev),71)
(kfib = fibptr->hw_fib_va,76)
(fibptr = aac_fib_alloc(dev),71)
(fibptr->hw_fib_va,76)
```


=======

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


# CVE-2016-6480 

### Description
- Race condition in the ioctl_send_fib function in drivers/scsi/aacraid/commctrl.c


Here we show how Joern helps you in the process of code auditing. The CVE bug concerns version 4.7 of the Linux Kernel.

### Setup
```bash
$ git clone https://github.com/torvalds/linux
$ cd linux
$ git checkout v4.7
```

This setup give us access to the vulnerable driver.

Build the CPG for the driver:

`./fuzzyc2cpg.sh path/to/kernel/linux/drivers/scsi/aacraid`


### Analysis

The first thing which comes to mind is the interaction from user to kernel space. This is usually done with `copy_from_user`. Let's see if any data from user space to kernel space is copied:

```scala
 cpg.call.name("copy_from_user").code.p
```

We should get the following output:

```
copy_from_user(&qd, arg, sizeof (struct aac_query_disk))
copy_from_user(&dd, arg, sizeof (struct aac_delete_disk))
copy_from_user(&dd, arg, sizeof (struct aac_delete_disk))
copy_from_user((void *)kfib, arg, sizeof(struct aac_fibhdr))
copy_from_user(kfib, arg, size)
copy_from_user((void *)&f, arg, sizeof(struct fib_ioctl))
copy_from_user(&fibsize, &user_srb->count,sizeof(u32))
copy_from_user(user_srbcmd, user_srb,fibsize)
copy_from_user(p,sg_user[i],upsg->sg[i].count)
copy_from_user(p,sg_user[i],upsg->sg[i].count)
copy_from_user(p,sg_user[i],usg->sg[i].count)
copy_from_user(p, sg_user[i],\n\t\t\t\t\t\t\tupsg->sg[i].count)
```

This doesn't look so bad.

Get some flows from `copy_from_user`:

```scala
val sinkArguments = cpg.method.name("copy_from_user").parameter.argument
println(sinkArguments.reachableByFlows(cpg.identifier).p)
```

Ok, this gives us a lot of information back. The following query gives us the 
number.

```scala
println(sinkArguments.reachableByFlows(cpg.identifier).l.size)
```

should be 302. 

Let's look for something which doesn't overwhelm us.
It would be interesting to have an estimate if the arguments of `copy_from_user`
are sanitized.

Since we don't have direct definitions at if expressions, we do not get any reaching definition information out of them. But we get information that flows into if expressions.

Maybe we can work our way around this. Fire up your *Main.scala* and add the following lines:

```scala
val reachingDefs1 = cpg.method
                       .name("copy_from_user")
                       .parameter
                       .argument
                       .reachableBy(cpg.identifier)
                       .toSet
```

So far we have used `reachableByFlows` to construct and print out our flows.
Sometimes we do not want that much of a detail. With `reachableBy` we tell the engine that we are not interested in the details of the data flow paths, but rather want to know which of our *sources* are hit. At the end of the query we collect the sources that are hit into a set.

```scala
val reachingDefs2 = cpg.method
                       .name(".*less.*", ".*greater.*")
                       .parameter
                       .argument
                       .reachableBy(cpg.identifier)
                       .toSet
```

Let's take a break and dismantle this query:

-  We restrict the flows running to expressions that involve the *less* **or** *greater* keyword. Note that internally each binary operation (+,-,>,< etc.) is also treated as a function. So we look for its arguments.
-  Data dependency is tracked back to each identifier it hits and collected into a set.
  
Now we can check if we have an intersection between these two sets which
gives us an estimate on what arguments of `copy_from_user` might be sanitized.


```scala
reachingDefs1.intersect(reachingDefs2).foreach(elem => println(elem.code))
```

should give us:

```
kmalloc(fibsize, GFP_KERNEL)
kmalloc(actual_fibsize - sizeof(struct aac_srb)\n\t\t\t  + sizeof(struct sgmap), GFP_KERNEL)
user_srbcmd->sg
kfib->header
size = le16_to_cpu(kfib->header.Size) + sizeof(struct aac_fibhdr)
actual_fibsize = sizeof(struct aac_srb) - sizeof(struct sgentry) +\n\t\t((user_srbcmd->sg.count & 0xff) * sizeof(struct sgentry))
* upsg = (struct user_sgmap64*)&user_srbcmd->sg
i = 0
user_srbcmd->sg
usg = kmalloc(actual_fibsize - sizeof(struct aac_srb)\n\t\t\t  + sizeof(struct sgmap), GFP_KERNEL)
user_srbcmd->sg
* upsg = &user_srbcmd->sg
user_srbcmd = kmalloc(fibsize, GFP_KERNEL)
i = 0
fibsize = 0
aac_fib_alloc(dev)
user_srbcmd->sg.count
kfib = fibptr->hw_fib_va
kfib->header.Size
actual_fibsize - sizeof(struct aac_srb)\n\t\t\t  + sizeof(struct sgmap)
fibptr = aac_fib_alloc(dev)
i = 0
fibptr->hw_fib_va
i = 0

```

This is actually quite nice; we can see that most *potential* checks involve some kind of a *size* element (as we might expect). 

At the beginning of this section we saw some `copy_from_user` outputs.
Let's look at those that have `kfib` as their first argument. This decision is not made randomly. If we look at the output above we see that `kfib` is an interesting pointer which gives us access to an header and its size seems to have an involvement in a check: 
`kfib->header.Size`.

We can cofirm this in the source code (`commctrl.c:90)`:

```c
size = le16_to_cpu(kfib->header.Size) + sizeof(struct aac_fibhdr);
if (size < le16_to_cpu(kfib->header.SenderSize))
                      ........
```

We use the following query to filter for `copy_from_user` looking for `kfib` as an argument:

```scala 
cpg.call.name("copy_from_user").code(".*kfib.*").l
```
we could also do:

```scala
 cpg.call.name("copy_from_user").filter(call => call.argument.code(".*kfib.*")).l
```

Let's print them out:

```
cpg.call.name("copy_from_user")
.filter(call => call.argument.code(".*kfib.*"))
.l
.foreach(call => println(call.code))
```

Output:

```
copy_from_user((void *)kfib, arg, sizeof(struct aac_fibhdr))
copy_from_user(kfib, arg, size)
```

Nice, we have two of them. If we find flows from these sinks to a common ancestor which defines `kfib` and there is no other definition of `kfib` along our way we might have a double fetch.

```scala
 val cfu1 = cpg.call.name("copy_from_user")
   .code(".*kfib.*")
   .l
   .head
   .start
   .reachableBy(cpg.identifier)
   .toSet

  val cfu2 = cpg.call.name("copy_from_user")
    .code(".*kfib.*")
    .l
    .last
    .start
    .reachableBy(cpg.identifier)
    .toSet
    .intersect(cfu1)


   cfu2.foreach(elem => println(elem.code, elem.lineNumber.get))
```

This is a similar pattern as we did above with our reaching definitions to sanitizers.
You might wonder what the `start` does here. It basically tells our engine to start a 
fresh traversal *starting* at the given node. In this case we filtered with *head* and
*last* to get those nodes. Play around with this to grasp the idea.

Our output:

```
(aac_fib_alloc(dev),71)
(kfib = fibptr->hw_fib_va,76)
(fibptr = aac_fib_alloc(dev),71)
(fibptr->hw_fib_va,76)
```

We see that both `copy_from_user` calls indeed have a common ancestor which defines 
our `kfib` pointer:

`kfib = fibptr->hw_fib_va,76` (*line 76/77 in the source code*)


Now we can define a query that shows us flows to this specific sink:

```scala
val cfu4 = cpg.call.name("copy_from_user")
    .code(".*kfib.*")
    .l
    .last
    .start
    .reachableByFlows(cpg.call.code("kfib = fibptr->hw_fib_va"))

println(cfu4.p)
```

Output:

```
| tracked                                    | lineNumber| method        | file                                  |
 |===============================================================================================================|
 | kfib = fibptr->hw_fib_va                   | 76        | ioctl_send_fib| linux/drivers/scsi/aacraid/commctrl.c|
 | kfib->header                               | 91        | ioctl_send_fib| linux/drivers/scsi/aacraid/commctrl.c|
 | kfib->header.SenderSize                    | 91        | ioctl_send_fib| linux/drivers/scsi/aacraid/commctrl.c|
 | le16_to_cpu(kfib->header.SenderSize)       | 91        | ioctl_send_fib| linux/drivers/scsi/aacraid/commctrl.c|
 | size = le16_to_cpu(kfib->header.SenderSize)| 91        | ioctl_send_fib| linux/drivers/scsi/aacraid/commctrl.c|
 | copy_from_user(kfib, arg, size)            | 115       | ioctl_send_fib| linux/drivers/scsi/aacraid/commctrl.c|


 __________________________________________________________________________________________________________________________
 | tracked                                             | lineNumber| method        | file                                 |
 |========================================================================================================================|
 | kfib = fibptr->hw_fib_va                            | 76        | ioctl_send_fib| linux/drivers/scsi/aacraid/commctrl.c|
 | kfib->header                                        | 91        | ioctl_send_fib| linux/drivers/scsi/aacraid/commctrl.c|
 | kfib->header.SenderSize                             | 91        | ioctl_send_fib| linux/drivers/scsi/aacraid/commctrl.c|
 | le16_to_cpu(kfib->header.SenderSize)                | 91        | ioctl_send_fib| linux/drivers/scsi/aacraid/commctrl.c|
 | size = le16_to_cpu(kfib->header.SenderSize)         | 91        | ioctl_send_fib| linux/drivers/scsi/aacraid/commctrl.c|
 | pci_alloc_consistent(dev->pdev, size, &daddr)       | 100       | ioctl_send_fib| linux/drivers/scsi/aacraid/commctrl.c|
 | kfib = pci_alloc_consistent(dev->pdev, size, &daddr)| 100       | ioctl_send_fib| linux/drivers/scsi/aacraid/commctrl.c|
 | copy_from_user(kfib, arg, size)                     | 115       | ioctl_send_fib| linux/drivers/scsi/aacraid/commctrl.c|


                                                                        .......

```
The second output shows us that there is a definition in between, but the first output reveals that we can reach our goal. 

Cool! We have a **double fetch**. Let's see if we can make this work in our favor.

Our output above shows that the issue is in the `ioctl_send_fib` function. Let's query for local variables in this function:

```scala
val localsFib = cpg.method.name("ioctl_send_fib").local.l 
localsFib.foreach(localVar => println(localVar.code))
```

Output:

```
hw_fib
fibptr
kfib
retval
size
daddr
```

We have seen that `size` is sanitized and dependent on `kfib->header.Size`.
The next query sheds light on this:

```scala
 val header = cpg.call.code("kfib->header.Size").l
 header.foreach(element => println(element.code, element.lineNumber.get))
```

Output:

```
(kfib->header.Size,89)
(kfib->header.Size,129)
```

We only have two entries. By looking into the source, we see that 
the first one at `89` is the one that is used to specifiy the size.
The second one at `129` is used **without any checks**.  
Note that the line numbers in the output of Joern do not necessarily match the linenumbers of the source code, but as you see they are close to the real ones.

Let's have a look into the source code [`commctrl.c`:121::132]:

``` scala
if (kfib->header.Command == cpu_to_le16(TakeABreakPt)) {
 .....
} else {
    retval = aac_fib_send(le16_to_cpu(kfib->header.Command), fibptr,
            le16_to_cpu(kfib->header.Size) , FsaNormal,
            1, 1, NULL, NULL);
```

In fact this is the racing bug which is specified in the CVE description. :)
