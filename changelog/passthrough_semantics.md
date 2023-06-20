# Pass Through Semantics & Update to Named Arguments (v1.2.20; June 2023)

PR: https://github.com/joernio/joern/pull/2885

## Summary

We have implemented the `PassThroughMapping` as a shorthand way to denote a method behaviour that:

* Doesn't cross taint parameters
* All parameters may affect the return
* Parameter numbers may be unbounded, e.g. `varargs`-style parameters

Examples of these are Python's tuple literals, where these can have a dynamic number of parameters, and they do not
cross-taint. This aims at reducing the number of false-positives that result from cross-tainting and unbounded
parameters.

In addition to this, the initial argument name feature of the flow definitions seemed a bit cumbersome, with one
requiring defining two flows for the same parameter. e.g. 1 -> 1, "foo" -> 1. This change now forces the user to define
both the parameter index and an optional name, i.e., you can no longer just have an argument name. This makes the 
summary more "method"-centric and less "caller"-centric. Argument names still have precedence, so if argument index is 
not appropriate due to type unpacking used by the method, then it may be left as some arbitrary value.

Note: `PASSTHROUGH` this does not taint 0 -> 0, but can be combined with other mappings.

## New Semantic Notation

```
"method" 1 -> 2, "foo" -> 2, 1 -> "bar", "foo" -> "bar"
"<operator>.tupleLiteral" 1 -> 1, 2 -> 2, 3 -> 3, 4 -> 4, 1 -> -1, 2 -> -1, 3 -> -1, 4 -> -1  // etc.
```

### New Semantic Notation

```
"method" 1 "foo" -> 2 "bar"
"<operator>.tupleLiteral" PASSTHROUGH
"setProperty" PASSTHROUGH 0 -> 0
```