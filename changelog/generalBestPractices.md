# Summary
This document contains some best practices and general remarks about the performance
of various common tasks, as well as best practices for joern devolopment.

This document exists because we have at various points encountered these issues.

Some points are very general to programming -- but if something is observed as a common
pitfall inside joern, then it deserves to be addressed here.


## Folds

Suppose we have `input: Iterable[List[T]]` and want to construct the concatenation.

A natural way could be
```
val res = input.foldLeft(Nil){_ ++ _}
```
This is a catastrophy: It boils down to
```
( ((Nil ++ input(0)) ++ input(1)) ++ input(2) ) ++ ...
```
However, the runtime of `A ++ B` scales like `A.length` for linked list, and this can
end up costing us `res.length ** 2`, i.e. quadratic runtime.

Instead, the correct way is
```
val res = input.foldRight(Nil){_ ++ _}
```
This is, however, contingent on the internals of the List implementation! If we 
instead were to collect into an Arraybuffer, the correct way would be
```
input.foldLeft(mutable.ArrayBuffer.empty[T]){case (acc, items) => acc.appendAll(items)}
```
This is because List allows O(1) prepend, and ArrayBuffer allows O(1) append.

Do not write `buf.appendedAll` -- that pulls a copy and runtime will always be quadratic!

Now, if you write `input.fold`, then the associativity is indeterminate. This means that
you must always assume the worst imaginable execution order!

### Fundamental Theorem
The fundamental theorem on getting the right complexity class for your folds is the following:

Assume that each item has a length, and that 
`combine(left, right).weight == left.weight + right.weight`, and assume that the runtime of
`combine(left, right)` is bounded by `min(left.weight, right.weight)`.

Then, the total runtime of accumulation is upper bounded by 
`input.map{_.weight}.sum * log2(input.map{_.weight}.sum)`.

Proof: Consider the function `F(a)=a*log2(a)` and then track the evolution of 
```time_already_spent - remaining_work.map{F(_.weight)}.sum```
We will show that this quantity is non-increasing. Since this quantity is initially negative
(no time was spent!) it must be negative once we are done, which gives the desired equation
```
time_spent - F(remaining_work.map{_.weight}.sum) == time_spent - F(output.weight) < 0
```
So to prove this inductively, suppose we combine two items with weights `a <= b`. We 
compare the critical quantity before and after this update, to obtain
```
    Delta == time_already_spent_after - remaining_work_after.map{F(_.weight)}.sum
             -  time_already_spent_before + remaining_work_before.map{F(_.weight)}.sum
          <= a - F(a+b) + F(a) + F(b) 
          == a  - a*log2(a+b) - b*log2(a+b) + a*log2(a) + b log2(b)
          == a - a*log2(1 + b/a) - b*log2(1 + a/b)
          <  a - a*log2(1 + b/a)
          <= 0
```
The last inequality was because `a <= b` and hence `log2(1+b/a) >= 1`.

### More examples

A good example of what not to do is java's `Collectors.toList()`. This is intended for
use on parallel streams; it uses an internal ArrayList. However, `ArrayList` only permits
fast append, no fast prepend. Yeah, lol, quadratic runtime (because associativity depends
on races).

To see how it should be done is seen in ForkJoinParallelCpgPass. Morally speaking, the relevant
function is
```
def combine[T](left:mutable.ArrayDeque[T], right:mutable.ArrayDeque[T]):mutable.ArrayDeque[T] = {
    if(left.size < right.size) right.prependAll(left)
    else left.appendAll(right)
}
```
This is the fundamental point: If you want `fold` to be fast, then you must handle both cases.

Another example that works is `++` for scala `Vector`. It is a fun exercise to step over the
code and see that both prependedAll and appendedAll are fast!

With respect to performance, it is recommended to use mutable datastructures by default, and
only use all the immutable stuff if necessary: That is, if you would otherwise require locks
or if your algorithm requires snapshots.

### Java stream collect
It is worthwhile to take another look at the java stream collector. It is used in CpgPass like this

```
//parts:Array[T]
          externalBuilder.absorb(
            java.util.Arrays
              .stream(parts)
              .parallel()
              .collect(
                new Supplier[DiffGraphBuilder] {
                  override def get(): DiffGraphBuilder =
                    new DiffGraphBuilder
                },
                new BiConsumer[DiffGraphBuilder, AnyRef] {
                  override def accept(builder: DiffGraphBuilder, part: AnyRef): Unit =
                    runOnPart(builder, part.asInstanceOf[T])
                },
                new BiConsumer[DiffGraphBuilder, DiffGraphBuilder] {
                  override def accept(leftBuilder: DiffGraphBuilder, rightBuilder: DiffGraphBuilder): Unit =
                    leftBuilder.absorb(rightBuilder)
                }
              )
          )
```          
The stream collect API is, at its core, a glorified parallel fold. Noteworthy things are:
1. We don't supply a single accumulator, we supply an accumulator factory. This is important for parallelism, otherwise we'd degrade to a foldLeft!
2. Ideally we only get one accumulator per cpu core. Each accumulator uses its `runOnPart` to absorb the output of the next `part`. We especially don't
allocate an accumulator (i.e. a new DiffGraphBuilder) for every `part` -- that would limit us to parallelisms where each `part` is large (e.g. each 
`method`) and would be bad for the case of many cheap `parts`.
3. Note how we collect everything into an array before building the stream. We do _not_ use some kind of generic SplitIterator like `java.util.Spliterators.spliteratorUnknownSize`.
Read the code of that function and consider whether that would be a good idea in the context of CpgPass.
4. The code for merging, i.e. `leftBuilder.absorb(rightBuilder)` has already been discussed above (especially the fact that it is ArrayDeque-based).

### API considerations: Beware of Iterator!
You should not offer a generic API that works on (lazy) Iterator. If you decide to do that, then immediately and single-threadedly collect the 
Iterator, before passing it on to complex higher order functions like fold or stream-collect.

The reason is that iterators are lazy. You don't know what side-effects and computations are hidden in them! And if execution of the iterator is triggered by 
complex higher order functions, then this may very well contain data races!

Or look at some code that we found in joern: `iter.map{item => executer.submit{() => expensive_function(item)}}.map{_.get()}.toList`, where `iter` was an Iterator.
 This code wanted to run `expensive_function` in parallel on all items in the iterator, and collect the results into a list.

In fact this code was single-threaded, because iterators and maps of iterators are lazy. So the first time something will be called is when `toList` tries to collect the first
output; then it schedules the first computations, awaits its result, and only then schedules the second computation.

TLDR: Iterators are scary. Collect them eagerly.