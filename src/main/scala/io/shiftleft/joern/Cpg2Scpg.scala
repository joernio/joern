package io.shiftleft.joern

import io.shiftleft.SerializedCpg
import io.shiftleft.layers.EnhancementRunner

object Cpg2Scpg {

  /**
    * Load the CPG at `cpgFilename` and add enhancements,
    * turning the CPG into an SCPG.
    * @param cpgFilename the filename of the cpg
    * */
  def run(cpgFilename: String): Unit = {
    val cpg = CpgLoader.load(cpgFilename)
    val serializedCpg = new SerializedCpg(cpgFilename)
    new EnhancementRunner().run(cpg, serializedCpg)
    serializedCpg.close
  }

}
