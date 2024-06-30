package io.joern.ghidra2cpg.passes

import ghidra.program.model.listing.{Function, Program}
import ghidra.program.util.DefinedDataIterator
import io.joern.ghidra2cpg._
import io.joern.ghidra2cpg.utils.Utils._
import io.joern.ghidra2cpg.utils.{Decompiler, PCodeMapper}
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.{NewBlock, NewMethod}
import io.shiftleft.codepropertygraph.generated.{EdgeTypes, nodes}
import io.shiftleft.passes.ForkJoinParallelCpgPass

import scala.jdk.CollectionConverters._
import scala.language.implicitConversions

class PCodePass(currentProgram: Program, fileName: String, functions: List[Function], cpg: Cpg, decompiler: Decompiler)
    extends ForkJoinParallelCpgPass[Function](cpg) {

  val address2Literals: Map[Long, String] = DefinedDataIterator
    .definedStrings(currentProgram)
    .iterator()
    .asScala
    .toList
    .map(x => x.getAddress().getOffset -> x.getValue.toString)
    .toMap
  def handleParameters(diffGraphBuilder: DiffGraphBuilder, function: Function, methodNode: NewMethod): Unit = {
    if (function.isThunk)
      function
        .getThunkedFunction(true)
        .getParameters
        .zipWithIndex
        .foreach { case (parameter, index) =>
          val node = createParameterNode(
            parameter.getName,
            parameter.getName,
            index + 1,
            parameter.getDataType.getName,
            function.getEntryPoint.getOffsetAsBigInteger.intValue()
          )
          diffGraphBuilder.addNode(node)
          diffGraphBuilder.addEdge(methodNode, node, EdgeTypes.AST)
        }
    else
      decompiler
        .toHighFunction(function)
        .get
        .getLocalSymbolMap
        .getSymbols
        .asScala
        .toSeq
        .filter(_.isParameter)
        .foreach { parameter =>
          val checkedParameterName = Option(parameter.getStorage)
            .flatMap(x => Option(x.getRegister))
            .flatMap(x => Option(x.getName))
            .getOrElse(parameter.getName)
          val node =
            createParameterNode(
              checkedParameterName,
              checkedParameterName,
              parameter.getCategoryIndex + 1,
              parameter.getDataType.getName,
              function.getEntryPoint.getOffsetAsBigInteger.intValue()
            )
          diffGraphBuilder.addNode(node)
          diffGraphBuilder.addEdge(methodNode, node, EdgeTypes.AST)
        }
  }

  def handleLocals(diffGraphBuilder: DiffGraphBuilder, function: Function, blockNode: NewBlock): Unit = {
    function.getLocalVariables.foreach { local =>
      val localNode = nodes
        .NewLocal()
        .name(local.getName)
        .code(local.toString)
        .typeFullName(Types.registerType(local.getDataType.toString))
      val identifier =
        createIdentifier(local.getName, local.getSymbol.getName, -1, local.getDataType.toString, -1)

      diffGraphBuilder.addNode(localNode)
      diffGraphBuilder.addNode(identifier)
      diffGraphBuilder.addEdge(blockNode, localNode, EdgeTypes.AST)
      diffGraphBuilder.addEdge(blockNode, identifier, EdgeTypes.AST)
      diffGraphBuilder.addEdge(identifier, localNode, EdgeTypes.REF)
    }
  }
  def handleBody(
    diffGraphBuilder: DiffGraphBuilder,
    function: Function,
    methodNode: NewMethod,
    blockNode: NewBlock
  ): Unit = {
    // Map instructions to nodes
    val instructionNodes = currentProgram.getListing
      .getInstructions(function.getBody, true)
      .iterator()
      .asScala
      .toList
      .map { instruction =>
        val highFunction = decompiler.toHighFunction(function).orNull
        new PCodeMapper(diffGraphBuilder, instruction, functions, highFunction, address2Literals).getNode
      }
    // Adding all nodes to the graph
    instructionNodes.foreach(diffGraphBuilder.addNode)
    // connections between nodes
    if (instructionNodes.nonEmpty) {
      diffGraphBuilder.addEdge(blockNode, instructionNodes.head, EdgeTypes.AST)
      diffGraphBuilder.addEdge(methodNode, instructionNodes.head, EdgeTypes.CFG)
      instructionNodes.sliding(2).foreach { nodes =>
        val prevInstructionNode = nodes.head
        val instructionNode     = nodes.last
        diffGraphBuilder.addEdge(blockNode, instructionNode, EdgeTypes.AST)
        diffGraphBuilder.addEdge(prevInstructionNode, instructionNode, EdgeTypes.CFG)
      }
    }
  }

  override def runOnPart(diffGraphBuilder: DiffGraphBuilder, function: Function): Unit = {
    val localDiffGraph = Cpg.newDiffGraphBuilder
    // we need it just once with default settings
    val blockNode  = nodes.NewBlock().code("").order(0)
    val methodNode = createMethodNode(decompiler, function, fileName, checkIfExternal(currentProgram, function.getName))
    val methodReturn = createReturnNode()

    localDiffGraph.addNode(methodNode)
    localDiffGraph.addNode(blockNode)
    localDiffGraph.addEdge(methodNode, blockNode, EdgeTypes.AST)
    localDiffGraph.addNode(methodReturn)
    localDiffGraph.addEdge(methodNode, methodReturn, EdgeTypes.AST)

    handleParameters(diffGraphBuilder, function, methodNode)
    handleLocals(diffGraphBuilder, function, blockNode)
    handleBody(diffGraphBuilder, function, methodNode, blockNode)
    diffGraphBuilder.absorb(localDiffGraph)
  }

  override def generateParts(): Array[Function] = functions.toArray
}
