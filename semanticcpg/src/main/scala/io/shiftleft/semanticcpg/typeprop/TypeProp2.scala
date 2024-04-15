package io.shiftleft.semanticcpg.typeprop

import io.shiftleft.semanticcpg.typeprop.TypeProp2.SymFn

import scala.collection.mutable

case class InputsAndTransferFunc[Point](inputs: collection.Seq[Point],
                                        transferFunc: () => Option[String])

trait TypeSystem[Point] {
  var pointToFullName: mutable.HashMap[Point, SymFn] = _
  var fullNameToSym: mutable.HashMap[SymFn, Sym] = _
  var nameToFullName: mutable.HashMap()

  def inputsOff(output: Point): InputsAndTransferFunc[Point]
}

trait ExternalSymbolProvider {
  def getSymbol(fullName: SymFn): Option[Sym]
}

class Sym(val fullName: SymFn, retSym: SymFn, val paramFullNames: Array[SymFn]) {

}

case class StackElement[Point](output: Point,
                               inputsAndTransFunc: InputsAndTransferFunc[Point]
                       )

class TypeProp2[Point](typeSystem: TypeSystem[Point],
                       externalSymbolProvider: ExternalSymbolProvider) {

  private val propagationStack = mutable.Stack.empty[StackElement[Point]]
  private val pointToFullName = mutable.HashMap.empty[Point, SymFn]
  private val fullNameToSymbol = mutable.HashMap.empty[SymFn, Sym]

  typeSystem.pointToFullName = pointToFullName
  typeSystem.fullNameToSym = fullNameToSymbol

  def run(starts: collection.Seq[Point]): Unit = {
    val startsIt = starts.iterator

    while (startsIt.hasNext) {
      if (propagationStack.isEmpty) {
        val output = startsIt.next()
        propagationStack.push(elementFromOutput(output))
      }

      while (propagationStack.nonEmpty) {
        val element = propagationStack.head
        val (fulfilled, notFulfilled) = element.inputsAndTransFunc.inputs.partition(pointToFullName.contains)

        if (notFulfilled.isEmpty) {
          propagationStack.pop()
          val symbolFullName = element.inputsAndTransFunc.transferFunc()
          pointToFullName.put(element.output, symbolFullName.orNull)
        } else {
          notFulfilled.foreach { notFulfilled =>
            propagationStack.push(elementFromOutput(notFulfilled))
          }
        }
      }
    }
  }

  private def elementFromOutput(output: Point): StackElement[Point] = {
    val inputsAndTransFunc = typeSystem.inputsOff(output)
    new StackElement(output, inputsAndTransFunc)
  }
}

object TypeProp2 {
  // Symbol full name
  type SymFn = String
}
