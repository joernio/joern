package io.joern.ghidra2cpg

import ghidra.app.decompiler.{DecompInterface, DecompileOptions, DecompileResults, DecompiledFunction}
import ghidra.program.model.listing.{Function, Program}
import ghidra.program.model.pcode.HighFunction

import scala.collection.mutable

object Decompiler {

  /** Create a new decompiler. Returns Some(decompiler) on success on None on failure.
    */
  def apply(program: Program): Option[Decompiler] = {
    val decompilerInterface = new DecompInterface()
    decompilerInterface.setSimplificationStyle("decompile")
    val opts = new DecompileOptions()
    opts.grabFromProgram(program)
    decompilerInterface.setOptions(opts)
    if (!decompilerInterface.openProgram(program)) {
      println("Decompiler error: %s\n", decompilerInterface.getLastMessage)
      None
    } else {
      Some(new Decompiler(decompilerInterface))
    }
  }
}

/** Interface to the ghidra decompiler, which performs caching to ensure that functions are not decompiled more than
  * once.
  */
class Decompiler(val decompInterface: DecompInterface) {

  val timeoutInSeconds                             = 60
  val cache: mutable.Map[String, DecompileResults] = mutable.Map()

  /** Retrieve HighFunction for given function, using the cache.
    */
  def toHighFunction(function: Function): Option[HighFunction] =
    decompile(function).map(_.getHighFunction)

  /** Retrieve DecompiledFunction for given function, using the cache.
    */
  def toDecompiledFunction(function: Function): Option[DecompiledFunction] =
    decompile(function).map(_.getDecompiledFunction)

  /** Decompile the given function, retrieving it from a cache if possible. Returns Some(highFunction) on success and
    * None on error.
    */
  private def decompile(function: Function): Option[DecompileResults] = {
    val addr = function.getEntryPoint.toString(true)
    cache.get(addr) match {
      case Some(x) =>
        Option(x)
      case None =>
        decompInterface.decompileFunction(function, timeoutInSeconds, null) match {
          case null =>
            cache.put(addr, null)
            None
          case x =>
            cache.put(addr, x)
            Some(x)
        }
    }
  }
}
