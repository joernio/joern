package io.shiftleft.joern

import io.shiftleft.SerializedCpg
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.cpgloading.CpgLoaderConfig
import io.shiftleft.dataflowengine.layers.dataflows.{OssDataFlow, OssDataFlowOptions}
import java.nio.file.{FileSystems, Files, Paths}

import io.shiftleft.codepropertygraph.generated.EdgeTypes
import io.shiftleft.overflowdb.OdbConfig
import io.shiftleft.semanticcpg.layers.LayerCreatorContext

import scala.jdk.CollectionConverters._

/**
  * Thin wrapper around `codepropertygraph`'s CpgLoader
  **/
object CpgLoader {

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
    if (semanticsFilenameOpt.isDefined) {
      removeAllSemantics(cpg)
      val semanticsFilename = semanticsFilenameOpt.getOrElse(defaultSemanticsFile)
      val context = new LayerCreatorContext(cpg, new SerializedCpg())
      val options = new OssDataFlowOptions(semanticsFilename)
      new OssDataFlow(() => options).run(context)
    }
  }

  /**
    * Undo `applySemantics`. This method is O(n) in the number of edges,
    * and single threaded, so consider it may take a bit for large graphs.
    * */
  def removeAllSemantics(cpg: Cpg): Unit = {
    val edgeTypesToRemove = Set(EdgeTypes.PROPAGATE, EdgeTypes.REACHING_DEF)
    // TODO Replace with call to generic DiffGraph unapply methods once available
    cpg.graph
      .edges()
      .asScala
      .filter(e => edgeTypesToRemove.contains(e.label))
      .foreach(_.remove)
  }

  /**
    * Load code property graph
    * @param filename name of the file that stores the cpg
    * @param storeFilename if unequal non-empty - location of ODB store
    * */
  def load(filename: String, storeFilename: String = ""): Cpg = {
    val config = if (storeFilename != "") {
      val odbConfig = OdbConfig.withDefaults().withStorageLocation(storeFilename)
      CpgLoaderConfig().withOverflowConfig(odbConfig)
    } else {
      CpgLoaderConfig()
    }
    io.shiftleft.codepropertygraph.cpgloading.CpgLoader.load(filename, config)
  }

  /**
    * Load code property graph from overflowDB and apply semantics
    * @param filename name of the file that stores the cpg
    * */
  def loadFromOdb(filename: String): Cpg = {
    val odbConfig = OdbConfig.withDefaults().withStorageLocation(filename)
    val config = CpgLoaderConfig().withOverflowConfig(odbConfig).doNotCreateIndexesOnLoad
    io.shiftleft.codepropertygraph.cpgloading.CpgLoader.loadFromOverflowDb(config)
  }

}
