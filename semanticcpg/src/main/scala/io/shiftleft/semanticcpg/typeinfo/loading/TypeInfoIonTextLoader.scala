package io.shiftleft.semanticcpg.typeinfo

import com.amazon.ion.{IonReader, IonType}
import com.amazon.ion.system.IonReaderBuilder

import scala.annotation.tailrec
import scala.util.Using

/** Loads text-formatted Ion from bytes or a string */
object TypeInfoIonTextLoader {
  def loadFromString(data: String): TypeDecl = {
    Using.resource(IonReaderBuilder.standard().build(data))(loop(_))
  }

  def loadFromBytes(data: Array[Byte]): TypeDecl = {
    Using.resource(IonReaderBuilder.standard().build(data))(loop(_))
  }

  private def loop(reader: IonReader, typ: TypeDecl = TypeDecl()): TypeDecl = {
    val ty = Option(reader.next())
    ty match
      case None                 => typ
      case Some(IonType.STRING) => loop(reader, parseTextField(reader, typ))
      case Some(IonType.STRUCT) => {
        // Toplevel struct/object
        reader.stepIn()
        loop(reader, typ)
      }
      case Some(IonType.LIST) => {
        val fieldName = reader.getFieldName()
        reader.stepIn()
        fieldName match
          case "INHERITS" => {
            val is = parseStringListContainer(reader)
            reader.stepOut()
            loop(reader, typ.copy(inherits = typ.inherits ::: is))
          }
          case "TYPE_PARAMETERS" => {
            val tps = parseStringListContainer(reader)
            reader.stepOut()
            loop(reader, typ.copy(typeParams = typ.typeParams ::: tps))
          }
          case "METHODS" => {
            val methods = parseStructList(reader, r => parseMethod(r))
            reader.stepOut()
            loop(reader, typ.copy(methods = methods))
          }
          case "MEMBERS" => {
            val members = parseStructList(reader, r => parseMember(r))
            reader.stepOut()
            loop(reader, typ.copy(members = members))
          }
      }
      case Some(_) => {
        throw new IllegalArgumentException(
          s"Failed to parse type info text, did not expect IonType: $ty: ${reader.stringValue()}"
        )
      }
  }

  private def parseStructList[T](reader: IonReader, structParser: IonReader => T): List[T] =
//    Option(r.next()) match
//      case Some(_) => structParser(r) :: parseStructList(r, structParser)
//      case None => List()
    {
      var result: List[T] = Nil
      while (reader.next() != null) {
        reader.stepIn()
        result = result :+ structParser(reader)
        reader.stepOut()
      }
      result
    }

  private def parseTextField(reader: IonReader, typ: TypeDecl): TypeDecl = {
    reader.getFieldName() match
      case "FULL_NAME" => typ.copy(fullName = reader.stringValue())
      case "NAME"      => typ.copy(name = reader.stringValue())
  }

  @tailrec
  private def parseMethod(reader: IonReader, method: Method = Method()): Method = {
    Option(reader.next()) match
      case None => method
      case Some(_) =>
        reader.getFieldName() match
          case "NAME"      => parseMethod(reader, method.copy(name = reader.stringValue()))
          case "FULL_NAME" => parseMethod(reader, method.copy(fullName = reader.stringValue()))
          case "SIGNATURE" => parseMethod(reader, method.copy(signature = reader.stringValue()))
  }

  @tailrec
  private def parseMember(reader: IonReader, member: Member = Member()): Member = {
    Option(reader.next()) match
      case None => member
      case Some(_) =>
        reader.getFieldName() match
          case "NAME"           => parseMember(reader, member.copy(name = reader.stringValue()))
          case "TYPE_FULL_NAME" => parseMember(reader, member.copy(typeFullName = reader.stringValue()))
  }

  @tailrec
  private def parseStringListContainer(reader: IonReader, strs: List[String] = Nil): List[String] = {
    Option(reader.next()) match
      case None                 => strs
      case Some(IonType.STRING) => parseStringListContainer(reader, reader.stringValue() :: strs)
  }
}
