package io.shiftleft.joern

import io.shiftleft.SerializedCpg
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.cpgloading.CpgLoaderConfig
import io.shiftleft.dataflowengine.layers.dataflows.DataFlowRunner
import io.shiftleft.dataflowengine.semanticsloader.SemanticsLoader
import java.nio.file.{FileSystems, Files, Paths}

import io.shiftleft.codepropertygraph.generated.EdgeTypes

import scala.jdk.CollectionConverters._

/**
  * Thin wrapper around `codepropertygraph`'s CpgLoader
  **/
object CpgLoader {

  /**
    * Load code property graph
    * @param filename name of the file that stores the cpg
    * */
  def load(filename: String, semanticsFilenameOpt: Option[String] = None): Cpg = {
    val cpg = loadWithoutSemantics(filename)
    applySemantics(cpg, semanticsFilenameOpt)
    cpg
  }

  lazy val defaultSemanticsFile: String = {
    val file = Files.createTempFile("joern-default", ".semantics")
    val defaultFile = this.getClass.getClassLoader.getResource("default.semantics").toURI

    // Weird, yes, but necessary when running as a distribution.
    // See (https://docs.oracle.com/javase/7/docs/technotes/guides/io/fsp/zipfilesystemprovider.html)
    if (defaultFile.getScheme.contains("jar")) {
      FileSystems.newFileSystem(defaultFile, Map("create" -> "false").asJava)
    }

    val fileLines = Files.readAllLines(Paths.get(defaultFile))
    Files.write(file, fileLines, java.nio.charset.StandardCharsets.UTF_8).toString
  }

  /**
    * Apply data flow semantics from `semanticsFilenameOpt` to `cpg`. If `semanticsFilenameOpt`
    * is omitted or None, default semantics will be applied.
    * */
  def applySemantics(cpg: Cpg, semanticsFilenameOpt: Option[String] = None): Unit = {
    val semanticsFilename = semanticsFilenameOpt.getOrElse(defaultSemanticsFile)
    val semantics = new SemanticsLoader(semanticsFilename).load
    new DataFlowRunner(semantics).run(cpg, new SerializedCpg())
  }

  /**
    * Undo `applySemantics`. This method is O(n) in the number of edges,
    * and single threaded, so consider it may take a bit for large graphs.
    * */
  def removeAllSemantics(cpg: Cpg): Unit = {
    val edgeTypesToRemove = Set(EdgeTypes.PROPAGATE, EdgeTypes.REACHING_DEF)
    // TODO Does overflowDB allow doing this in parallel?
    cpg.scalaGraph
      .E()
      .toIterator()
      .filter(e => edgeTypesToRemove.contains(e.label))
      .foreach(_.remove)
  }

  /**
    * Load code property graph but do not apply semantics
    * @param filename name of the file that stores the cpg
    * */
  def loadWithoutSemantics(filename: String): Cpg = {
    val config = CpgLoaderConfig()
    io.shiftleft.codepropertygraph.cpgloading.CpgLoader.load(filename, config)
  }

}
