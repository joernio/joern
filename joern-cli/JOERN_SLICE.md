# Joern Slice

`JoernSlice` is the entrypoint for `joern-slice` and specifies ways to extract useful subsets of information from the
CPG. Two modes are available:

* **Data-flow**: This is a pretty standard backwards data-flow slicing command that starts at call arguments and slices 
backwards to create a graph of slices.
* **Usages**: This targets locals and parameters and traces what calls they make and in which calls they are used. This is
useful for describing how a variable interacts in a procedure.

Each slicer outputs JSON, which allows the result to be ingested by other processes or libraries e.g. NetworkX.

### General

There are shared options between all options:

```
  cpg                      input CPG file name, or source code - defaults to `cpg.bin`
  -o, --out <value>        the output file to write slices to - defaults to `slices`. The file is suffixed based on the mode.
  --dummy-types            for generating CPGs that use type recovery, enables the use of dummy types - defaults to false.
  --file-filter <value>    the name of the source file to generate slices from.
  --method-name-filter <value>
                           filters in slices that go through specific methods by names. Uses regex.
  --method-parameter-filter <value>
                           filters in slices that go through methods with specific types on the method parameters. Uses regex.
  --method-annotation-filter <value>
                           filters in slices that go through methods with specific annotations on the methods. Uses regex.
```

### Data-Flow

This is interprocedural and the paths are only limited by a depth argument with a default of 20. Note this is expensive
so note the filter options. This creates a graph with an additional mapping to denote the methods under which each node
belongs:

```
Command: data-flow [options]

  --slice-depth <value>    the max depth to traverse the DDG for the data-flow slice - defaults to 20.
  --sink-filter <value>    filters on the sink's `code` property. Uses regex.
  --end-at-external-method
                           all slices must end at an external method - defaults to false.
```

#### Schema

```scala
case class DataFlowSlice(nodes: Set[SliceNode], edges: Set[SliceEdge])

case class SliceNode(
  id: Long,
  label: String,
  name: String = "",
  code: String,
  typeFullName: String = "",
  parentMethod: String = "",
  parentFile: String = "",
  lineNumber: Option[Integer] = None,
  columnNumber: Option[Integer] = None
)

case class SliceEdge(src: Long, dst: Long, label: String)
```

### Usages

The usages slice describes how a variable interacts within its procedure. This is perhaps a more "descriptive" slice
in some ways. The variables are locals and parameters and the referencing identifiers are tracked to find what the
variable calls and what calls it forms an argument of.

Accompanying this slice, there is an optional source code property. With these details one can generate a useful LLM
prompts, for example.

```
Command: usages [options]

  --min-num-calls <value>  the minimum number of calls required for a usage slice - defaults to 1.
  --exclude-operators      excludes operator calls in the slices - defaults to false.
  --exclude-source         excludes method source code in the slices - defaults to false.
```

#### Schema

```scala
case class ProgramUsageSlice(objectSlices: List[MethodUsageSlice], userDefinedTypes: List[UserDefinedType])

case class MethodUsageSlice(
  code: String,
  fullName: String,
  fileName: String,
  slices: Set[ObjectUsageSlice],
  lineNumber: Option[Int] = None,
  columnNumber: Option[Int] = None
)

case class ObjectUsageSlice(
  targetObj: DefComponent,
  definedBy: Option[DefComponent],
  invokedCalls: List[ObservedCall],
  argToCalls: List[ObservedCallWithArgPos]
)

sealed trait DefComponent {
  def name: String

  def typeFullName: String

  def label: String

  def lineNumber: Option[Int]

  def columnNumber: Option[Int]
}
// ^ See the LocalDef, LiteralDef, ParamDef, CallDef, and UnknownDef under `io.joern.dataflowengineoss.slicing.package`

case class ObservedCall(
  callName: String,
  resolvedMethod: Option[String],
  paramTypes: List[String],
  returnType: String,
  lineNumber: Option[Int] = None,
  columnNumber: Option[Int] = None
)

case class ObservedCallWithArgPos(
  callName: String,
  resolvedMethod: Option[String],
  paramTypes: List[String],
  returnType: String,
  position: Either[String, Int],
  lineNumber: Option[Int] = None,
  columnNumber: Option[Int] = None
)

case class UserDefinedType(
  name: String,
  fields: List[LocalDef],
  procedures: List[ObservedCall],
  fileName: String = "",
  lineNumber: Option[Int] = None,
  columnNumber: Option[Int] = None
)
```
