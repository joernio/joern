package io.joern.x2cpg.frontendspecific.rubysrc2cpg

import io.joern.x2cpg.passes.frontend.XTypeHintCallLinker
import io.joern.x2cpg.frontendspecific.rubysrc2cpg.Constants.*
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.{Call, NewMethod}
import io.shiftleft.semanticcpg.language.*

import java.util.regex.Pattern

class RubyTypeHintCallLinker(cpg: Cpg) extends XTypeHintCallLinker(cpg) {

  override def calls: Iterator[Call] = super.calls.nameNot("^(require).*")

  private val fileNamePattern = Pattern.compile("^(.*(.rb)).*$")

  override def createMethodStub(methodName: String, call: Call, builder: DiffGraphBuilder): NewMethod = {
    // In the case of Python/JS we can use name info to check if, despite the method name might be incorrect, that we
    // label the method correctly as internal by finding that the method should belong to an internal file
    val matcher  = fileNamePattern.matcher(methodName)
    val basePath = cpg.metaData.root.head
    val isExternal = if (matcher.matches()) {
      val fileName = matcher.group(1)
      cpg.file.nameExact(s"$basePath$fileName").isEmpty
    } else {
      true
    }
    val name =
      if (methodName.contains(pathSep) && methodName.length > methodName.lastIndexOf(pathSep) + 1)
        val strippedMethod = methodName.stripPrefix(s"$kernelPrefix.")
        if kernelFunctions.contains(strippedMethod) then strippedMethod
        else methodName.substring(methodName.lastIndexOf(pathSep) + pathSep.length)
      else methodName
    createMethodStub(name, methodName, call.argumentOut.size, isExternal, builder)
  }
}
