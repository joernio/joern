+++
Title="More examples"
weight=7
+++

## Example - CVE-2016-6480 (Linux Kernel)

A race condition exists in the Linux Kernel in version 4.7 in the ioctl_send_fib in `drivers/scsi/aacraid/commctrl.c`.

### Setup
```bash
$ git clone https://github.com/torvalds/linux
$ cd linux
$ git checkout v4.7
```

We build the code property graph for the vulnerable driver as follows:
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
