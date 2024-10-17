package io.shiftleft.semanticcpg.typeinfo.loading

import com.amazon.ion.{IonReader, IonType}
import com.amazon.ion.system.IonReaderBuilder
import io.shiftleft.semanticcpg.typeinfo.{PackageMetadata, TypeMetadata}

import scala.annotation.tailrec
import scala.util.Using

object MetadataIonTextLoader {
  def loadFromBytes(data: Array[Byte]): PackageMetadata = {
    Using.resource(IonReaderBuilder.standard().build(data))(parseTopLevelStruct)
  }

  def loadRawVersionsFromBytes(data: Array[Byte]): List[String] = {
    Using.resource(IonReaderBuilder.standard().build(data))(parseOnlyVersions)
  }

  private def parseOnlyVersions(reader: IonReader): List[String] = {
    reader.next()
    reader.stepIn()
    parseUntil(reader, reader => reader.getFieldName == "VERSIONS")
    reader.stepIn()
    parseStringList(reader)
  }

  @tailrec
  private def parseUntil(reader: IonReader, condition: IonReader => Boolean): Unit = {
    Option(reader.next()) match
      case Some(IonType.LIST) if condition(reader) => ()
      case None => throw new RuntimeException("Invalid type info package metadata format: no versions lists")
      case _    => parseUntil(reader, condition)
  }

  /** The metadata ion struct is two named top-level lists: a list of version strings a list of type info structs
    * containing the version and a list of all type names Unused at the moment, will be used when writing details to be
    * committed to the repo
    */
  private def parseTopLevelStruct(reader: IonReader): PackageMetadata = {
    reader.next()
    reader.stepIn()
    val packageMetadata = dispatchTopLevelList(reader, PackageMetadata())
    dispatchTopLevelList(reader, packageMetadata)
  }

  private def dispatchTopLevelList(reader: IonReader, packageMetadata: PackageMetadata): PackageMetadata = {
    Option(reader.next()) match
      case Some(IonType.LIST) => {
        reader.stepIn()
        val newPackageMetadata: PackageMetadata = reader.getFieldName match {
          case "VERSIONS"       => packageMetadata.copy(versions = parseStringList(reader))
          case "TYPE_NAME_INFO" => parseTypeNameInfo(reader, packageMetadata)
        }
        reader.stepOut()
        newPackageMetadata
      }
      case _ => throw new RuntimeException("Invalid type info package metadata format")
  }

  @tailrec
  private def parseTypeNameInfo(reader: IonReader, packageMetadata: PackageMetadata): PackageMetadata = {
    Option(reader.next()) match
      case Some(IonType.STRUCT) => {
        reader.stepIn()
        val typeInfo = parseTypeInfoStruct(reader, parseTypeInfoStruct(reader))
        reader.stepOut()
        val newPackageMetadata =
          packageMetadata.copy(perVersionTypeInfo = packageMetadata.perVersionTypeInfo :+ typeInfo)
        parseTypeNameInfo(reader, newPackageMetadata)
      }
      case None => packageMetadata
      case _ =>
        throw new RuntimeException("Invalid type info package metadata format: type info list has invalid format")
  }

  private def parseTypeInfoStruct(reader: IonReader, typeMetadata: TypeMetadata = TypeMetadata()): TypeMetadata = {
    Option(reader.next()) match
      case Some(IonType.STRING) => typeMetadata.copy(version = reader.stringValue())
      case Some(IonType.LIST) => {
        reader.stepIn()
        val typeNames = parseStringList(reader)
        reader.stepOut()
        typeMetadata.copy(typeNames = typeNames)
      }
      case _ =>
        throw new RuntimeException("Invalid type info package metadata format: type info struct has invalid format")
  }

  @tailrec
  private def parseStringList(reader: IonReader, elts: List[String] = Nil): List[String] = {
    Option(reader.next()) match
      case None                 => elts
      case Some(IonType.STRING) => parseStringList(reader, elts :+ reader.stringValue())
      case _                    => throw new RuntimeException("Invalid type info package metadata format")
  }
}
