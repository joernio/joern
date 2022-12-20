# Data Flow Engine

A taint-tracking system based on whole-program data-dependence 
representation. External library calls can be defined by 
semantic models (see `io.joern.dataflowengineoss.DefaultSemantics`).

Basic usage:

```scala
// If using Joern shell, imports and engine context will be pre-configured and available already
import io.shiftleft.semanticcpg.language._
import io.joern.dataflowengineoss.language.toExtendedCfgNode

def sink = cpg.call.argument.code(".*malicious_input.*")
def source = cpg.call(".*println.*")

// Traverses data flow in the backwards direction
sink.reachableBy(source)
```

## Configuration

To begin using the data flow engine on the CPG, we need the following:

```scala
  // (1) Imports to extend CFG nodes
  import io.joern.dataflowengineoss.language.toExtendedCfgNode
  import io.joern.dataflowengineoss.queryengine.{EngineContext, EngineConfig}

  import scala.util.{Failure, Success, Try}
  import scala.io.{BufferedSource, Source}

  // (2) Optional: Configure the engine
  val engineConfig = EngineConfig(maxCallDepth = 2, initialTable = None, disableCacheUse = false)

  // (3) Create execution context for the engine
  implicit var context: EngineContext = EngineContext(config =  engineConfig)
```