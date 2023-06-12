package io.joern.rubysrc2cpg.utils

import java.nio.file.{Files, Paths}
import scala.collection.mutable

case class MethodTableModel(methodName: String, parentClassPath: String)

case class PackageContext(moduleName: String, packageTable: PackageTable)

class PackageTable() {

  private val methodTableMap = mutable.HashMap[String, mutable.HashSet[MethodTableModel]]()

  private val packageCallMap = mutable.HashMap[String, mutable.HashSet[String]]()

  def addPackageMethod(moduleName: String, methodName: String, parentClassPath: String): Unit = {
    val packageMethod = MethodTableModel(methodName, parentClassPath)
    methodTableMap.getOrElseUpdate(moduleName, mutable.HashSet.empty[MethodTableModel]) += packageMethod
  }

  def getPackageMethod(moduleName: String, methodName: String): Set[MethodTableModel] = {
    methodTableMap.getOrElse(moduleName, Set.empty[MethodTableModel]).toSet.filter(_.methodName == methodName)
  }

  def addPackageCall(sourceFileName: String, moduleName: String): Unit = {
    packageCallMap.getOrElseUpdate(sourceFileName, mutable.HashSet.empty[String]) += moduleName
  }

  def getPackageCallInFile(fileName: String): Set[String] = {
    packageCallMap.getOrElse(fileName, Set.empty[String]).toSet
  }

  def checkIfInternalDependency(fileName: String, methodName: String): Boolean = {
    (!packageCallMap.contains(fileName) || !packageCallMap(fileName).contains(methodName)) && methodTableMap.contains(
      fileName
    ) && methodTableMap(fileName).exists(_.methodName == methodName)
  }
}

object PackageTable {
  def resolveImportPath(modulePath: String): String = {
    val pathValue = modulePath.replaceAll("'", "").replaceAll("\"", "")
    val result = pathValue match {
      case path if Files.isRegularFile(Paths.get(path)) =>
        path
      case path if Files.isRegularFile(Paths.get(path + ".rb")) =>
        s"${path}.rb"
      case _ =>
        pathValue
    }
    result
  }
}
