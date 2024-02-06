package io.joern.rubysrc2cpg.deprecated.utils

import java.io.File as JFile
import java.util.regex.Pattern
import scala.collection.mutable

case class MethodTableModel(methodName: String, parentClassPath: String, classType: String)
case class ModuleModel(name: String, fullName: String)
case class TypeDeclModel(name: String, fullName: String)
case class PackageContext(moduleName: String, packageTable: PackageTable)

class PackageTable {

  val methodTableMap  = mutable.HashMap[String, mutable.HashSet[MethodTableModel]]()
  val moduleMapping   = mutable.HashMap[String, mutable.HashSet[ModuleModel]]()
  val typeDeclMapping = mutable.HashMap[String, mutable.HashSet[TypeDeclModel]]()

  def addPackageMethod(moduleName: String, methodName: String, parentClassPath: String, classType: String): Unit = {
    val packageMethod = MethodTableModel(methodName, parentClassPath, classType)
    methodTableMap.getOrElseUpdate(moduleName, mutable.HashSet.empty[MethodTableModel]) += packageMethod
  }

  def addModule(gemOrFileName: String, moduleName: String, modulePath: String): Unit = {
    val fName = gemOrFileName.split(Pattern.quote(JFile.separator)).lastOption.getOrElse(gemOrFileName)
    moduleMapping.getOrElseUpdate(gemOrFileName, mutable.HashSet.empty[ModuleModel]) += ModuleModel(
      moduleName,
      s"$fName::program.$modulePath"
    )
  }

  def addTypeDecl(gemOrFileName: String, typeDeclName: String, typeDeclPath: String): Unit = {
    val fName = gemOrFileName.split(Pattern.quote(JFile.separator)).lastOption.getOrElse(gemOrFileName)
    typeDeclMapping.getOrElseUpdate(gemOrFileName, mutable.HashSet.empty[TypeDeclModel]) += TypeDeclModel(
      typeDeclName,
      s"$fName::program.$typeDeclPath"
    )
  }

  def getMethodFullNameUsingName(
    packageUsed: List[String] = List(PackageTable.InternalModule),
    methodName: String
  ): List[String] =
    packageUsed
      .filter(methodTableMap.contains)
      .flatMap {
        case PackageTable.InternalModule =>
          methodTableMap(PackageTable.InternalModule)
            .filter(_.methodName == methodName)
            .map(method => s"${method.parentClassPath}.$methodName")
        case module =>
          methodTableMap(module)
            .filter(_.methodName == methodName)
            .map(method => s"$module::program:${method.parentClassPath}$methodName")
      }

  def getPackageInfo(moduleName: String): List[MethodTableModel] = {
    methodTableMap.get(moduleName) match
      case Some(value) => value.toList
      case None        => List.empty[MethodTableModel]
  }

  def getModule(gemOrFileName: String): List[ModuleModel] = {
    moduleMapping.get(gemOrFileName) match
      case Some(value) => value.toList
      case None        => List.empty[ModuleModel]
  }

  def getTypeDecl(gemOrFileName: String): List[TypeDeclModel] = {
    typeDeclMapping.get(gemOrFileName) match
      case Some(value) => value.toList
      case None        => List.empty[TypeDeclModel]
  }

  def set(table: PackageTable): Unit = {
    methodTableMap.addAll(table.methodTableMap)
    moduleMapping.addAll(table.moduleMapping)
    typeDeclMapping.addAll(table.typeDeclMapping)
  }
  def clear(): Unit = {
    methodTableMap.clear
    moduleMapping.clear
    typeDeclMapping.clear
  }
}

object PackageTable {
  val InternalModule = "<internal>"
}
