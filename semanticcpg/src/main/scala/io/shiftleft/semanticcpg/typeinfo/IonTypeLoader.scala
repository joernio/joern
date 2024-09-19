package io.shiftleft.semanticcpg.typeinfo

import com.amazon.ion.{IonReader, IonType}
import com.amazon.ion.system.IonReaderBuilder

import java.io.IOException
import scala.annotation.tailrec
import scala.util.{Failure, Try, Using};

object IonTypeLoader extends Loader {
  def parse(data: String): Try[TypeDecl] = {
    val reader = IonReaderBuilder.standard().build(data)
    val typ = Try(loop(reader))
    for {
      _ <- Try(reader.close())
      typ <- typ
    } yield
      typ
//    try {
//      reader.close()
//    } catch {
//      case e => Failure(e)
//    } finally {
//      reader.close()
//    }
//    val typ = Try(loop(reader))
//    r.close()
//    typ
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
          case "DEPENDS" => {
            val ds = parseDependencies(r)
            r.stepOut()
            loop(r, typ.copy(dependencies = typ.dependencies ::: ds))
          }
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
  private def parseDependency(r: IonReader, d: Dependency = Dependency()): Dependency =
    Option(r.next()) match
      case None => d
      case Some(_) =>
        r.getFieldName() match
          case "FULL_NAME" => parseDependency(r, d.copy(fullName = r.stringValue()))
          case "VERSION" => parseDependency(r, d.copy(version = Some(r.stringValue())))

  @tailrec
  private def parseDependencies(r: IonReader, ds: List[Dependency] = List()): List[Dependency] =
    Option(r.next()) match
      case None => ds
      case Some(IonType.STRUCT) => {
        r.stepIn()
        val dep = parseDependency(r)
        r.stepOut()
        parseDependencies(r, ds :+ dep)
      }

  @tailrec
  private def parseStringListContainer(r: IonReader, strs: List[String] = List()): List[String] =
    Option(r.next()) match
      case None => strs
      case Some(IonType.STRING) => parseStringListContainer(r, r.stringValue() :: strs)
}