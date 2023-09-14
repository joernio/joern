## 

The handling of access paths was designed and implemented by Bernhard Brehm (bbrehm@shiftleft.io).

## Access Path Algebra without relations

Classically, access paths are modeled as a roughly a "free monoid": A path is a list of strings, including one special "VariableAccess" token. Member access appends to this path; either the field name, or the VariableAccess token "?" for non-literal index accesses. It is not possible to invert elements in this list; these are the only supported ways of deriving one object reference from another, and two derived references are assumed to be equal if and only if they have identical access paths.

We also have a notion of "contexts": The access path of a context describes a set of access paths; we encode this by storing a prefix, and a set of exclusions. Then, this set describes the set of reachable paths starting from there. Let's call this a context path.

Now, if we track some base symbol with given context path CP, and encounter a function call with access path AP, we need to split: We continue to track with CP \ AP, i.e. we continue to track everything reachable through CP that is not reachable through AP. The remainder, CP \intersect AP, is translated and tracked into the function call. Very often, this will not find any changes, and we will emerge with CP \intersect AP again.

We split in a non-disjoint way for VarableAccess tokens: I.e. we overtaint. The path is tracked into the function, but not excluded at all.

Due to the "free monoid" style of paths, the intersection is easy: CP \intersect AP is either empty, or CP is a prefix of AP: Then it is AP itself (plus exclusions), or AP is a prefix of CP: Then it is CP itself, and CP \ AP is empty.

In this model, we don't need any special handling for the "deref" / "*" operator: It is simply an access path token like any other.

## Complex Access Path Elements

Now, we want to handle four complications.

1. the AddressOf operator, which has the special property that it cancels with the Deref operator. I.e. *(&x) is identical to x itself. Depending on modeling decisions, we may also say that &(*x) is equal to x itself. For simplicity, we will write all pathes in postfix, i.e. x: & * and x: * &.

2. the PointerShift operator, which we will write as p: <i>, and which corresponds to p+i. This again has a VariableAccess variant, such that p: <5> and p: <?> are valid paths. The property of the pointershift is that subsequent shifts collapse by addition, i.e. p: <5> <-3> <-2> * === p: *. Then, a C array access can be writen as p[4] === *(p+4) === p: <4> *.

3. the getelementptr ("GEP") construct. Consider C code like p->a: This is mostly equivalent to both (*p).a and *&(p->a); the subtle difference is that &p->a does not load from memory, it instead derives a pointer from p (C's addressof is not a function/operator, it is a syntactic construct). This is a very natural operation in LLVM (given a pointer to a struct, derive a pointer to a specific member; i.e. a LEA instruction). Now, we have two valid paths to the member p->a: We can first load the entire struct from main memory into register / stack, and then extract the element a, or we can derive a pointer &(p->a) to the struct element from p, and then load it. Both of these paths lead to the exact same element, but they look different. To be more precise, we have a conflict between `p: [GEP a] *` and `p: * [Extract a]`. Typical C code will prefer the `p: * [Extract a]` notation (closer to the source, i.e. C syntax!) while typical LLVM code will prefer `p: [GEP a] *` (closer to the source, i.e. llvm-IR; also, compilers prefer to emit this). Suppose that we have AddressOf working; then, we can obtain correct behavior by either:

    2.1 Logical viewpoint: `[Extract a] => a`, `[GEP a] => * a &`. Consider the example: We then get the two paths `p: * a` and `p: * a & *`, and these are equal (as long as `& *` cancels).
    3.2 Memory viewpoint: `[Extract a] => & a *`, `[GEP a] => a`. Consider the example: We then get the two paths `p: * & a *` and `p: a *`, and these are equal (as long as `* &` cancels).

A large consideration is whether we want to consider this as an invertible operation on paths. We currently tend to consider it as non-invertible: I.e. we can derive a field-pointer from a parent struct-pointer, but we cannot go back (it is of course theoretically possible to go back, but we don't want to support that).

4. The pointercast construct. Consider C code like `((myStruct*)p)->a`, or consider C unions. In these cases, pointers are reinterpreted into pointers of different types. This changes the meaning of subsequent path elements (load, GEP, pointershift). Realistically, our goal should be to overtaint most of the time, but still retain correct behavior independent of pointer base-type: I.e. if our base pointer is char*, but it is always used as myStruct* (i.e. always cast right before use), then we want to find the exact same flows as if it was a myStruct* pointer in the first place. Unfortunately we don't know how to model this nicely, yet.

## One-sided invertible AddressOf

We chose to handle complications 1-3 in the following way. We chose to make both `& *` and `* &` collapse, and hence use the logical viewpoint `[Extract a] => a`, `[GEP a] => * a &`. We propose that GEP, Extract and Shift become new built-in accessPath modifying functions, and AddressOf and Shift become new tokens for access paths. This way, we can change the backend representation in the future, without needing front-ends to change their emissions. 

The elements `<LiteralInteger>` and `<?>` and `&` are considered invertible, but `*` is considered non-invertible (even though, in reality, `*` is invertible and `<?>` is non-invertible). We will maintain the invariant that exclusions cannot contain the tokens `<?>` or `?`, and exclusions cannot have trailing invertible elements.

When we track a context path CP and encounter call with argument access path AP, from the same base, then we need to matchAndDiff. The general approach is the following:

 1. Normalize the paths: We collapse internal elements (`& *` -> nil, `<0>`->nil, `<i> <j>`->`<i+j>`, `<i> <?>`->`<?>`, `<?> <i>`->`<?>`), and split off all trailing invertible elements, into CP = CP_HEAD CP_INV and AP = AP_HEAD AP_INV (we can possibly just maintain this split all the time).

 2. Suppose AP_HEAD is a prefix or equal to CP_HEAD. That is, CP = AP_HEAD OVERHEAD CP_INV For example, we track base: `* <5> *`, and we encounter foo(`base: * <2>`). In this case, we don't split and track into the function. For this, we mentally rewrite our current path into CP = AP_HEAD AP_INV AP_INV^(-1) OVERHEAD CP_INV, and hence track into the call with translated path AP_INV^(-1) OVERHEAD CP_INV, which would be arg: `<-2> <5> *` = `arg: <3> *`. Now suppose that the call doesn't change anything: Then, we would prepend AP on returning, such that we come out tracking base: `* <2> <3> * = base: * <5> *`, just as it should be.

 3.  Suppose AP_HEAD is an extension of CP_HEAD. That is, AP = CP_HEAD EXT AP_INV. For example, we track `base: * <2>` and encounter `foo(base: * <5> * <1>)`. Then we mentally rewrite AP = CP_HEAD CP_INV CP_INV^(-1) EXT AP_INV, which would be `AP = (*) (<2>) (<-2>) (<5> *) (<1>)`. We now need to split: I.e. we need to check for the exclusion CP_INV^(-1) EXT, and track both into and around the call. The step-over flow gets an extended exclusion list, and the step-into call will track arg: , i.e. an empty path. If the extemsion is already excluded, i.e. if there exists an exclusion starting with CP_INV^(-1) EXT, then we don't track into, and simply go on. Otherwise, the ongoing flow gets the new exclusion added to its list, possibly removing (shadowing) some existing exclusions: If there existed an exclusion of the form CP_INV^(-1) EXT TRAIL, then it can be deleted because it is implied by the new exclusion. Further, we need to track into the function. For this, the tracked path is empty, with truncated exclusion list: We keep exclusions of the form CP_INV^(-1) EXT TRAIL, and truncate them to TRAIL. Note that this scheme preserves the fact that exclusions always terminate in a non-invertible symbol. In our example, suppose that `<3> *` was excluded, i.e. we originally tracked `base: * <2> \ <3> *`. We now encounter `foo(base: * <2> <-2> <5> *)`: This is excluded, and we don't need to track it. 
    
 Suppose on the other hand that we started with `base: * <2> \ <3> * *`. Then, we track into, with path `arg: \ *`, and continue tracking with exclusion `base: * <2> \ <3> *` (which shadows the old exclusion). If the call did not change anything, then we will come out with `arg: \ *` again, which translates back to `base: * <5> * \ *`. Naively this might look nonsensical-- we track a pointer, but we don't care about the pointee. However, it makes sense in view of arrays: For example, `base: * <5> * <1> *` is a perfectly valid access path that we still care about! This is due to the unfortunate fact that C and llvm don't properly distinguish between pointers to single objects and pointers to packed arrays.

By using this approach, we don't change any existing code paths, i.e. CPGs that don't use Shift or AddressOf are unchanged. However, it is incubent on frontends to properly translate code such that it does not rely on the non-existing cancellation `* &`. I.e. `&arr[5]` needs to be translated into `arr: <5>` by the frontend. This is not an undue burden: The addressof really must appear in the same expression, since it is a syntactic construct, not a function. `tmp = arr[5]; &tmp` is semantically different, and the addressof does not undo the dereference!

A further issue is that this approach is very unforgiving for policies. Say that e.g. someone wrote a policy for a function `void foo(int* src, int* dst){*dst = *src;}`. We can track through this function all right, but suppose its policy was a MAP override. If this was called like `foo(&someptr[5], other)`, then it would transfer the entire array to other, instead of just excluding / transfering the 5th element. Passing the entire array through the function is correct (it could be `dst[-2] = *src`), but the policy must tell us that it overrides the access path through *, i.e. must include the deref. Hence, we need to expand the policy language to permit such information.

## Further examples

Consider
```
void foo(T1 arg1, T2 arg2){
    arg1[1] = *arg2;    //return from policy with arg2: <0> *
                        //enter with LHS, continue with arg1: <3> \ <-3> *, <-2> *
                        //AP arg1: <3> <-3> <1> *, arg2: <0> *
                        //CP arg1: <3> \ <-3> *
}
...
q = source();   //find flow via q *
foo(p+2, q);    //return twice: [q: *] and [p: <2> <3> \ <-3> *, <-2> *]
                //enter with arg1: <3> \ <-3> *
                //AP p: <2>
                //CP p: <2> <-2> <5> \ <-3> *
foo(p[2], q);   //would enter with arg1: <3> * which is excluded
                //AP p <5> <-5> <2> *
                //CP p <5> \ <3> *  
p[2] = 0; // track: p: <5> \ <-3> *
sink(p+5); // track p: <5>
```
