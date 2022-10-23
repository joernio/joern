package io.joern.console

import java.io.OutputStream
import scala.collection.mutable

trait Reporting {

  def reportOutStream: OutputStream = System.err

  def report(string: String): Unit = {
    reportOutStream.write((string + "\n").getBytes("UTF-8"))
    GlobalReporting.appendToGlobalStdOut(string)
  }
}

/** A dirty hack to capture the reported output for the server-mode. Context: server mode is a bit tricky, because the
  * reporting happens inside the repl, but we want to retrieve it from the context _outside_ the repl, and the two have
  * separate classloaders. There's probably a cleaner way, but for now this serves our needs.
  *
  * Note that this convolutes the output from concurrently-running jobs - so we should not run UserRunnables
  * concurrently.
  */
object GlobalReporting {
  private var enabled = false

  def enable(): Unit =
    enabled = true

  def disable(): Unit =
    enabled = false

  private val globalStdOut = new mutable.StringBuilder

  def appendToGlobalStdOut(s: String): Unit = {
    if (enabled) globalStdOut.append(s + System.lineSeparator())
  }

  def getAndClearGlobalStdOut(): String = {
    val result = globalStdOut.result()
    globalStdOut.clear()
    result
  }
}
