package io.joern.ghidra2cpg.utils

import ghidra.app.decompiler.{DecompInterface, DecompileOptions, DecompileResults, DecompiledFunction}
import ghidra.program.model.listing.{Function, Program}
import ghidra.program.model.pcode.HighFunction

import scala.collection.immutable

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
      println(s"Decompiler error: ${decompilerInterface.getLastMessage}")
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

  val timeoutInSeconds        = 60
  @volatile private var cache = immutable.HashMap[String, DecompileResults]()

  /** Return the cache of decompile results. For debugging only.
    */
  def getCache: immutable.HashMap[String, DecompileResults] = this.cache

  /** Retrieve HighFunction for given function, using the cache.
    */
  def toHighFunction(function: Function): Option[HighFunction] =
    decompile(function).flatMap(decompiled => Option(decompiled.getHighFunction))

  /** Retrieve DecompiledFunction for given function, using the cache.
    */
  def toDecompiledFunction(function: Function): Option[DecompiledFunction] =
    decompile(function).flatMap(decompiled => Option(decompiled.getDecompiledFunction))

  /** Decompile the given function, retrieving it from a cache if possible. Returns Some(highFunction) on success and
    * None on error.
    */
  private def decompile(function: Function): Option[DecompileResults] = {
    // ghidra decompileFunction acquires a lock on decompInterface.
    // we can still run with many threads, using the cache.
    // we use double-checked locking with volatile copy-on-write hashmap as a cache
    val addr  = function.getEntryPoint.toString(true)
    val ccOld = this.cache
    ccOld.get(addr) match {
      case Some(null)       => return None
      case some @ Some(res) => return some
      case _                =>
    }
    this.synchronized {
      val ccCurrent = this.cache
      if (!(ccOld eq ccCurrent))
        ccCurrent.get(addr) match {
          case Some(null)       => return None
          case some @ Some(res) => return some
          case _                =>
        }
      val res = decompInterface.decompileFunction(function, timeoutInSeconds, null)
      this.cache = ccCurrent.updated(addr, res)
      Option(res)
    }
  }
}
