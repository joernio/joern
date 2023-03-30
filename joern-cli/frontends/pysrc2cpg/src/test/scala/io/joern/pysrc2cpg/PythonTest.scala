package io.joern.pysrc2cpg.cpg

import io.joern.pysrc2cpg.{DynamicTypeHintFullNamePass, ImportsPass, Py2CpgOnFileSystem, Py2CpgOnFileSystemConfig, PythonNaiveCallLinker, PythonTypeHintCallLinker, PythonTypeRecovery}
import io.shiftleft.codepropertygraph
import org.slf4j.LoggerFactory
import io.shiftleft.semanticcpg.language._
import better.files.File
import io.joern.dataflowengineoss.layers.dataflows.{OssDataFlow, OssDataFlowOptions}
import io.joern.x2cpg.X2Cpg
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.semanticcpg.layers.LayerCreatorContext

import java.util.Calendar
import scala.jdk.CollectionConverters.CollectionHasAsScala
import scala.util.{Failure, Success, Try}


object PythonTest extends App {
  val cpgOutFile = File.newTemporaryFile(suffix = ".cpg.bin")
  cpgOutFile.deleteOnExit()

  val absoluteSourceLocation = File("/Users/rahul/IdeaProjects/recommendation-system").path.toAbsolutePath

  val cpgconfig = Py2CpgOnFileSystemConfig(cpgOutFile.path, absoluteSourceLocation, File(".venv").path, true)
  val cpgT = new Py2CpgOnFileSystem().createCpg(cpgconfig)
  cpgT.foreach(
    cpg => {

      X2Cpg.applyDefaultOverlays(cpg)
      new ImportsPass(cpg).createAndApply()
      new PythonTypeRecovery(cpg).createAndApply()
      new PythonTypeHintCallLinker(cpg).createAndApply()
      // Apply OSS Dataflow overlay
      new OssDataFlow(new OssDataFlowOptions()).run(new LayerCreatorContext(cpg))
      //val callNodes = cpg.call.code(".*fetchImageUrl.*").l
      val callNodes = cpg.call.methodFullName("<unknownFullName>").name("as_matrix").lineNumber(44).l

      callNodes.foreach(f=>{
        println(s"Call node: ${f.code} ----> ${f.methodFullName}")
      }
      )
    }
  )
}
