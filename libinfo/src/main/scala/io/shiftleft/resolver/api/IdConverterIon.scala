package io.shiftleft.resolver.api

import com.amazon.ion.{IonReader, IonWriter}

trait IdConverterIon[I <: Id] {
  def readId(reader: IonReader): I
  def writeId(writer: IonWriter, id: I): Unit
}
