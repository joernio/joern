package io.shiftleft.semanticcpg.typeprop

import scala.collection.mutable

case class InputsAndTransferFunc[Point](inputs: collection.Seq[Point],
                                        transferFunc: (collection.Seq[Option[String]]) => Option[String])

trait LanguageInterface[Point] {
  def inputsOff(output: Point): InputsAndTransferFunc[Point]
}

trait SymbolProviderInterface {
  def getSymbol(fullName: String): Option[Sym]
}

class Sym(val fullName: String, retSym: String, paramSyms: Array[String]) {
  
}

case class StackElement[Point](output: Point,
                               inputsAndTransFunc: InputsAndTransferFunc[Point]
                       )

class TypeProp2[Point](language: LanguageInterface[Point]) {

  private val propagationStack = mutable.Stack.empty[StackElement[Point]]
  private val symbolFullNames = mutable.HashMap.empty[Point, String]

  def run(starts: collection.Seq[Point]): Unit = {
    val startsIt = starts.iterator

    while (startsIt.hasNext) {
      if (propagationStack.isEmpty) {
        val output = startsIt.next()
        propagationStack.push(elementFromOutput(output))
      }

      while (propagationStack.nonEmpty) {
        val element = propagationStack.head
        val (fulfilled, notFulfilled) = element.inputsAndTransFunc.inputs.partition(symbolFullNames.contains)

        if (notFulfilled.isEmpty) {
          propagationStack.pop()
          val symbolFullName = element.inputsAndTransFunc.transferFunc(fulfilled.map(f => Option(symbolFullNames.apply(f))))
          symbolFullNames.put(element.output, symbolFullName.orNull)
        } else {
          notFulfilled.foreach { notFulfilled =>
            propagationStack.push(elementFromOutput(notFulfilled))
          }
        }
      }
    }
  }

  private def elementFromOutput(output: Point): StackElement[Point] = {
    val inputsAndTransFunc = language.inputsOff(output)
    new StackElement(output, inputsAndTransFunc)
  }
}
