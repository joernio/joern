package io.joern.rubysrc2cpg.utils

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

case class MethodTableModel(methodName: String, parentClassPath: String, classType: String)

case class PackageContext(moduleName: String, packageTable: PackageTable)

class PackageTable() {

  private val methodTableMap = mutable.HashMap[String, mutable.HashSet[MethodTableModel]]()

  def addPackageMethod(moduleName: String, methodName: String, parentClassPath: String, classType: String): Unit = {
    val packageMethod = MethodTableModel(methodName, parentClassPath, classType)
    methodTableMap.getOrElseUpdate(moduleName, mutable.HashSet.empty[MethodTableModel]) += packageMethod
  }

  def getMethodFullNameUsingName(packageUsed: List[String], methodName: String): List[String] = {
    val finalMethodName = ListBuffer[String]()
    packageUsed.foreach(module => {
      if (methodTableMap.contains(module)) {
        methodTableMap(module)
          .filter(_.methodName == methodName)
          .foreach(method => {
            finalMethodName.addOne(s"$module.${method.parentClassPath}$methodName:<unresolvedSignature>")
          })
      }
    })
    if (finalMethodName.isEmpty) {
      List("<unknowfullname>")
    } else {
      finalMethodName.toList
    }
  }
}
