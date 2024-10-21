package io.joern.javasrc2cpg.util

class TemporaryNameProvider {

  val tmpNamePrefix         = "$obj"
  private var tmpIndex: Int = 0

  def next: String = {
    val name = s"$tmpNamePrefix$tmpIndex"
    tmpIndex += 1
    name
  }

}
