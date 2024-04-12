package io.joern.swiftsrc2cpg.testfixtures

import io.joern.dataflowengineoss.testfixtures.SemanticTestCpg
import io.joern.x2cpg.testfixtures.DefaultTestCpg
import io.shiftleft.semanticcpg.typeprop.{Sym, SymbolProviderInterface}
import io.shiftleft.semanticcpg.language._
import scala.collection.mutable

class SwiftDefaultTestCpg(val fileSuffix: String)
    extends DefaultTestCpg
    with SwiftSrc2CpgFrontend
    with SemanticTestCpg {

  override protected def applyPasses(): Unit = {
    super.applyPasses()
    applyOssDataFlow()
  }

  override protected def applyPostProcessingPasses(): Unit = {
    new TypeProp2(symbolProvider).run(this.methodReturn.b)
  }
  
  private val symbolProvider = new TestSymbolProvider()
  
  def addSymbol(symbol: String): Unit = {
    symbolProvider.addSymbol(symbol)
  }
}

class TestSymbolProvider() extends SymbolProviderInterface {
  
  private val fullNameToSymbol = mutable.HashMap.empty[String, Sym]
  override def getSymbol(fullName: String): Option[Sym] = {
    fullNameToSymbol.get(fullName)
  }
  
  def addSymbol(symbol: String): Unit = {
    val parts = symbol.split(',')
    val sym = new Sym(parts(0), parts(1), parts.takeRight(parts.length - 2))
    fullNameToSymbol.put(sym.fullName, sym)
  }
}
