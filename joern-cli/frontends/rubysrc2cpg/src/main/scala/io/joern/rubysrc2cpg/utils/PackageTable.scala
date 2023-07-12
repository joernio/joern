package io.joern.rubysrc2cpg.utils

import scala.collection.mutable

case class MethodTableModel(methodName: String, parentClassPath: String, classType: String)

case class PackageContext(moduleName: String, packageTable: PackageTable)

class PackageTable() {

  val methodTableMap = mutable.HashMap[String, mutable.HashSet[MethodTableModel]]()

  def addPackageMethod(moduleName: String, methodName: String, parentClassPath: String, classType: String): Unit = {
    val packageMethod = MethodTableModel(methodName, parentClassPath, classType)
    methodTableMap.getOrElseUpdate(moduleName, mutable.HashSet.empty[MethodTableModel]) += packageMethod
  }

  def getMethodFullNameUsingName(packageUsed: List[String], methodName: String): List[String] =
    packageUsed
      .filter(methodTableMap.contains)
      .flatMap(module =>
        methodTableMap(module)
          .filter(_.methodName == methodName)
          .map(method => s"$module::program:${method.parentClassPath}$methodName")
      )

  def getPackageInfo(moduleName: String): List[MethodTableModel] = {
    methodTableMap.get(moduleName) match
      case Some(value) => value.toList
      case None        => List[MethodTableModel]()
  }

  def set(table: PackageTable): Unit = {
    methodTableMap.addAll(table.methodTableMap)
  }
  def clear(): Unit = methodTableMap.clear
}
