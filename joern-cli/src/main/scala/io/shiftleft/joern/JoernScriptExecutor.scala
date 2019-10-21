package io.shiftleft.joern

import java.io.FileReader

import io.shiftleft.console.ScriptExecutor
import javax.script.{ScriptEngine, ScriptEngineManager}

class JoernScriptExecutor(cpgFilename: String = "cpg.bin.zip") extends ScriptExecutor {

  private val engine: ScriptEngine = new ScriptEngineManager().getEngineByName("scala")

  private val cpgLoadingCode: String =
    s"""
       | import io.shiftleft.joern.CpgLoader
       | import io.shiftleft.codepropertygraph.Cpg
       | import io.shiftleft.semanticcpg.language._
       | import io.shiftleft.dataflowengine.language._
       | val cpg : Cpg = CpgLoader.load("$cpgFilename")
       |""".stripMargin

  run(cpgLoadingCode)

  override def run(script: String): Object = engine.eval(script, engine.getContext)

  def run(script: FileReader): Object = engine.eval(script, engine.getContext)

}
