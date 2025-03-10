package io.shiftleft.resolver.util

import com.amazon.ion.{IonReader, IonType}

object IonUtil {
  def forNextValues(reader: IonReader, func: => Unit): Unit = {
    var valueType: IonType = null
    while ({
      valueType = reader.next;
      valueType != null
    }) {
      func
    }
  }
}
