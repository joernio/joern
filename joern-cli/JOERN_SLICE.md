# Joern Slice

`JoernSlice` is the entrypoint for `joern-slice` and specifies ways to extract useful subsets of information from the
CPG. Two modes are available:

* **Data-flow**: This is a pretty standard backwards data-flow slicing command that starts at call arguments and slices 
backwards to create a graph of slices.
* **Usages**: This targets locals and parameters and traces what calls they make and in which calls they are used. This is
useful for describing how a variable interacts in a procedure.

Each slicer outputs JSON, which allows the result to be ingested by other processes or libraries e.g. NetworkX.

### Data-Flow

This is interprocedural and the paths are only limited by a depth argument with a default of 20. Note this is expensive
so note the filter options. This creates a graph with an additional mapping to denote the methods under which each node
belongs:

```
Command: data-flow [options]

  --slice-depth <value>    the max depth to traverse the DDG for the data-flow slice - defaults to 20.
```

*TODO*: Add more filter options

#### Schema

```scala
case class DataFlowSlice(nodes: Set[SliceNode], edges: Set[SliceEdge], methodToChildNode: Map[String, Set[Long]])

case class SliceNode(
    id: Long,
    label: String,
    name: String = "",
    code: String,
    typeFullName: String = "",
    lineNumber: Integer = -1,
    columnNumber: Integer = -1
)

case class SliceEdge(src: Long, dst: Long, label: String)
```

### Usages

The usages slice describes how a variable interacts within its procedure. This is perhaps a more "descriptive" slice
in some ways. The variables are locals and parameters and the referencing identifiers are tracked to find what the
variable calls and what calls it forms an argument of.

Accompanying this slice, there is an optional source code property. With these details one can generate a useful LLM
prompts, for example.

*TODO*: Clean up schema

```
Command: usages [options]

  --min-num-calls <value>  the minimum number of calls required for a usage slice - defaults to 1.
  --exclude-operators      excludes operator calls in the slices - defaults to false.
  --exclude-source         excludes method source code in the slices - defaults to false.
```

#### Schema

```scala
case class ProgramUsageSlice(objectSlices: Map[String, MethodUsageSlice], userDefinedTypes: List[UserDefinedType])

case class MethodUsageSlice(source: String, slices: Set[ObjectUsageSlice])

case class ObjectUsageSlice(
                             targetObj: DefComponent,
                             definedBy: Option[DefComponent],
                             invokedCalls: List[ObservedCall],
                             argToCalls: List[(ObservedCall, Int)]
                           )

case class DefComponent(name: String, typeFullName: String, literal: Boolean = false)

case class ObservedCall(callName: String, paramTypes: List[String], returnType: String)

case class UserDefinedType(name: String, fields: List[DefComponent], procedures: List[ObservedCall])
```
