package io.shiftleft.joern

import io.shiftleft.SerializedCpg
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.cpgloading.CpgLoaderConfig
import io.shiftleft.layers.EnhancementRunner
import io.shiftleft.semanticsloader.SemanticsLoader

/**
  * Thin wrapper around `codepropertygraph`'s CpgLoader
  * */
object CpgLoader {

  /**
    * Load code property graph
    * @param filename name of the file that stores the cpg
    * @param semanticsFilenameOption file from which semantics are loaded. If not provided defaults are loaded.
    * */
  def load(filename: String, semanticsFilenameOption: Option[String] = None): Cpg = {
    val config = CpgLoaderConfig.default
    val cpg = io.shiftleft.codepropertygraph.cpgloading.CpgLoader.load(filename, config)


    val semanticsFilename = semanticsFilenameOption.getOrElse("src/main/resources/default.semantics")
    val semantics = new SemanticsLoader(semanticsFilename).load
    new EnhancementRunner(semantics).run(cpg, new SerializedCpg())

    cpg
  }

}
