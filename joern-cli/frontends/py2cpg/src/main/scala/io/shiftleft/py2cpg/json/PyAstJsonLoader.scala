package io.shiftleft.py2cpg.json

import io.bullet.borer.derivation.MapBasedCodecs._
import io.bullet.borer.{Codec, Json}
import io.shiftleft.py2cpg.pyast.PyAst._

import java.nio.charset.StandardCharsets
import scala.io.Source
import sys.process._

object PyAstJsonLoader extends App {
  implicit lazy val iconstantCodecs: Codec[iconstant] = deriveAllCodecs[iconstant]
  implicit lazy val expr_contextCodecs: Codec[iexpr_context] = deriveAllCodecs[iexpr_context]
  implicit lazy val boolopCodecs: Codec[iboolop] = deriveAllCodecs[iboolop]
  implicit lazy val operatorCodecs: Codec[ioperator] = deriveAllCodecs[ioperator]
  implicit lazy val unaryopCodecs: Codec[iunaryop] = deriveAllCodecs[iunaryop]
  implicit lazy val cmpopCodecs: Codec[icmpop] = deriveAllCodecs[icmpop]
  implicit lazy val comprehensionCodecs: Codec[icomprehension] = deriveAllCodecs[icomprehension]
  implicit lazy val excepthandlerCodecs: Codec[iexcepthandler] = deriveAllCodecs[iexcepthandler]
  implicit lazy val argumentsCodecs: Codec[iarguments] = deriveAllCodecs[iarguments]
  implicit lazy val argCodecs: Codec[iarg] = deriveAllCodecs[iarg]
  implicit lazy val keywordCodecs: Codec[ikeyword] = deriveAllCodecs[ikeyword]
  implicit lazy val aliasCodecs: Codec[ialias] = deriveAllCodecs[ialias]
  implicit lazy val withitemCodecs: Codec[iwithitem] = deriveAllCodecs[iwithitem]
  implicit lazy val type_ignoreCodecs: Codec[itype_ignore] = deriveAllCodecs[itype_ignore]

  implicit lazy val exprCodecs: Codec[iexpr] = deriveAllCodecs[iexpr]
  implicit lazy val stmtCodecs: Codec[istmt] = deriveAllCodecs[istmt]
  implicit lazy val modCodecs: Codec[imod] = deriveAllCodecs[imod]

  val file = args(0)
  val code = Source.fromFile(file).getLines().mkString("\n")

  var jsonAst: String = _

  val processBuilder = Process("python3.9 /home/ml/work/pyAst2Json/pyAst2Json.py")
  val io = BasicIO.standard(connectInput = true).withInput { outputStream =>
    val lenAndCode = code.length + "\n" + code
    val bytes = lenAndCode.getBytes(StandardCharsets.UTF_8)
    outputStream.write(bytes)
    outputStream.close()
  }.withOutput { inputStream =>
    val bufferedReader = Source.fromInputStream(inputStream).bufferedReader()
    val line = bufferedReader.readLine()
    val bytesToRead = line.toInt
    println("Bytes received: " + bytesToRead)
    val jsonAstAsArray = new Array[Char](bytesToRead)
    bufferedReader.read(jsonAstAsArray, 0, bytesToRead)

    jsonAst = String.copyValueOf(jsonAstAsArray)
  }


  val process = processBuilder.run(io)
  process.exitValue()

  //println(jsonAst)

  val ast = timed(Json.decode(jsonAst.getBytes(StandardCharsets.UTF_8)).to[imod].value)
  //println(ast)

  def timed[T](func:  => T): T = {
    val start = System.currentTimeMillis()
    val ret = func
    val stop= System.currentTimeMillis()
    println(s"Time: ${stop - start}")
    ret
  }


  /*
  val testAst: imod = Module(Try(Import(Alias("docutils")::Nil), Nil, Nil, Nil))
  val encodedTestAst = Json.encode(testAst).toUtf8String
  println(encodedTestAst)


  val testData = """{"Module":{"body":[{"Expr":{"value":{"Call":{"func":{"Name":{"id":"'print'","ctx":{"Load":{}},"lineno":1,"col_offset":0}},"args":[{"Constant":{"value":"'foo'","lineno":1,"col_offset":6}}],"keywords":[{"keyword":{"arg":"'x'","value":{"Constant":{"value":"1","lineno":1,"col_offset":17}}}}],"lineno":1,"col_offset":0}},"lineno":1,"col_offset":0}}],"type_ignores":[]}}
"""
  println(testData)



  println(Json.decode(testData.getBytes(StandardCharsets.UTF_8)).to[imod].value)
   */


}
