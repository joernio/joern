package io.shiftleft.semanticcpg.typeinfo

import com.amazon.ion.{IonReader, IonType}
import com.amazon.ion.system.IonReaderBuilder
import io.shiftleft.semanticcpg.typeinfo.loading.Loader

import java.io.{IOException, InputStream}
import scala.annotation.tailrec
import scala.util.{Failure, Try, Using};

object IonLoader extends Loader[TypeDecl] {
  override def parse(data: String): Try[TypeDecl] = 
    Using.Manager { use =>
      val reader = use(IonReaderBuilder.standard().build(data))
      loop(reader)
    }

  override def parse(data: Array[Byte]): Try[TypeDecl] =
    Using.Manager { use =>
      val reader = use(IonReaderBuilder.standard().build(data))
      loop(reader)
    }
  
  override def parseString(data: InputStream): Try[TypeDecl] =
    Using.Manager { use =>
      val reader = use(IonReaderBuilder.standard().build(data))
      loop(reader)
    }
    
  private def loop(r: IonReader, typ: TypeDecl = TypeDecl()): TypeDecl = {
    val ty = Option(r.next())
    ty match
      case None => typ
      case Some(IonType.STRING) => loop(r, parseTextField(r, typ))
      case Some(IonType.STRUCT) => {
        // Toplevel struct/object
        r.stepIn()
        loop(r, typ)
      }
      case Some(IonType.LIST) => {
        val fieldName = r.getFieldName()
        r.stepIn()
        fieldName match
          case "INHERITS" => {
            val is = parseStringListContainer(r)
            r.stepOut()
            loop(r, typ.copy(inherits = typ.inherits ::: is))
          }
          case "TYPE_PARAMETERS" => {
            val tps = parseStringListContainer(r)
            r.stepOut()
            loop(r, typ.copy(typeParams = typ.typeParams ::: tps))
          }
          case "METHODS" => {
            val methods = parseStructList(r, r => parseMethod(r))
            r.stepOut()
            loop(r, typ.copy(methods = methods))
          }
          case "MEMBERS" => {
            val members = parseStructList(r, r => parseMember(r))
            r.stepOut()
            loop(r, typ.copy(members = members))
          }
      }
      case Some(_) => {
        throw new IllegalArgumentException(s"Failed to parse type info text, did not expect IonType: $ty: ${r.stringValue()}")
      }
  }
  
  private def parseStructList[T](r: IonReader, structParser: IonReader => T): List[T] =
//    Option(r.next()) match
//      case Some(_) => structParser(r) :: parseStructList(r, structParser)
//      case None => List()
        
    {
    var result: List[T] = List()
    while (r.next() != null) {
      r.stepIn()
      result = result :+ structParser(r)
      r.stepOut()
    }
    result
  }

  private def parseTextField(r: IonReader, typ: TypeDecl): TypeDecl =
    r.getFieldName() match
      case "FULL_NAME" => typ.copy(fullName = r.stringValue())
      case "NAME" => typ.copy(name = r.stringValue())

  @tailrec
  private def parseMethod(r: IonReader, m: Method = Method()): Method =
    Option(r.next()) match
      case None => m
      case Some(_) =>
        r.getFieldName() match
          case "NAME" => parseMethod(r, m.copy(name = r.stringValue()))
          case "FULL_NAME" => parseMethod(r, m.copy(fullName = r.stringValue()))
          case "SIGNATURE" => parseMethod(r, m.copy(signature = r.stringValue()))

  @tailrec
  private def parseMember(r: IonReader, m: Member = Member()): Member =
    Option(r.next()) match
      case None => m
      case Some(_) =>
        r.getFieldName() match
          case "NAME" => parseMember(r, m.copy(name = r.stringValue()))
          case "TYPE_FULL_NAME" => parseMember(r, m.copy(typeFullName = r.stringValue()))

  @tailrec
  private def parseStringListContainer(r: IonReader, strs: List[String] = List()): List[String] =
    Option(r.next()) match
      case None => strs
      case Some(IonType.STRING) => parseStringListContainer(r, r.stringValue() :: strs)
}