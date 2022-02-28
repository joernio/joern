package io.shiftleft.py2cpg

class AutoIncIndex(private var index: Int) {
  def getAndInc: Int = {
    val ret = index
    index += 1
    ret
  }
}
