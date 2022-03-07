package io.joern.pysrc2cpg

class AutoIncIndex(private var index: Int) {
  def getAndInc: Int = {
    val ret = index
    index += 1
    ret
  }
}
