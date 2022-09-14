package io.joern.console

import java.io.OutputStream

trait Reporting {
  // TODO in scala3, make this a trait parameter (rather than abstract member)
  def reportOutStream: OutputStream

  def report(string: String): Unit =
    reportOutStream.write(string.getBytes("UTF-8"))
}
