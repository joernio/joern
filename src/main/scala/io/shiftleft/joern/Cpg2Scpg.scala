package io.shiftleft.joern

import io.shiftleft.SerializedCpg
import io.shiftleft.layers.EnhancementRunner
import io.shiftleft.semanticsloader.SemanticsLoader

object Cpg2Scpg {

  /**
    * Load the CPG at `cpgFilename` and add enhancements,
    * turning the CPG into an SCPG.
    * @param cpgFilename the filename of the cpg
    * @param semanticsFilenameOption the filename of the semantics file
    * */
  def run(cpgFilename: String, semanticsFilenameOption: Option[String] = None): Unit = {
    val cpg = CpgLoader.load(cpgFilename)
    val semanticsFilename = semanticsFilenameOption.getOrElse("src/main/resources/default.semantics")
    val semantics = new SemanticsLoader(semanticsFilename).load
    val serializedCpg = new SerializedCpg(cpgFilename)
    new EnhancementRunner(semantics).run(cpg, serializedCpg)
    serializedCpg.close
  }

}
