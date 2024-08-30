package io.shiftleft.semanticcpg.typeinfo

import com.amazon.ion.{IonReader, IonType}
import com.amazon.ion.system.IonReaderBuilder
import org.slf4j.{Logger, LoggerFactory}

object Loader {
  private val logger: Logger = LoggerFactory.getLogger("IonLoader")

  def readFromString(data: String): String = {
    val builder = IonReaderBuilder.standard()
    val reader = builder.build(data)
    val txt = parse(reader)
    reader.close()
    txt
  }
  
  private def parse(r: IonReader, sb: StringBuilder = StringBuilder()): String = {
    val ty = Option(r.next())
    ty match
      case Some(ty: IonType) => {
        if (IonType.isText(ty)) {
          val field = r.getFieldName()
          val value = r.stringValue()
          parse(r, sb.append(s"$field:$value,"))
        } else if (IonType.isContainer(ty)) {
          r.stepIn()
          parse(r, sb.append(s"Container,"))
        } else {
          parse(r, sb.append(s"Other,"))
        }
      }
      case None => sb.toString()
  }
}
