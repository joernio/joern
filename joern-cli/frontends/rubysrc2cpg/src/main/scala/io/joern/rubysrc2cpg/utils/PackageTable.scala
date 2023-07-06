package io.joern.rubysrc2cpg.utils

import io.joern.rubysrc2cpg.astcreation.AstCreatorHelper
import io.joern.x2cpg.Defines.DynamicCallUnknownFullName

import java.util.concurrent.ConcurrentHashMap
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

case class MethodTableModel(methodName: String, parentClassPath: String, classType: String)

case class PackageContext(moduleName: String, packageTable: PackageTable)

class PackageTable() extends AstCreatorHelper {

  private val methodTableMap = new ConcurrentHashMap[String, mutable.HashSet[MethodTableModel]]()

  def addPackageMethod(moduleName: String, methodName: String, parentClassPath: String, classType: String): Unit = {
    val packageMethod = MethodTableModel(methodName, parentClassPath, classType)
    val moduleMethodSet = methodTableMap.synchronized {
      methodTableMap.computeIfAbsent(moduleName, _ => mutable.HashSet.empty[MethodTableModel])
    }
    moduleMethodSet.add(packageMethod)
  }

  def getMethodFullNameUsingName(packageUsed: List[String], methodName: String): Option[String] = {
    if (isExcluded(methodName)) {
      Some(DynamicCallUnknownFullName)
    } else {
      val finalMethodName = ListBuffer[String]()
      packageUsed.foreach(module => {
        if (methodTableMap.containsKey(module)) {
          methodTableMap
            .get(module)
            .filter(_.methodName == methodName)
            .foreach(method => {
              finalMethodName.addOne(s"$module::program:${method.parentClassPath}:$methodName")
            })
        }
      })

      finalMethodName.headOption
    }
  }
}
