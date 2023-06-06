package io.joern.rubysrc2cpg.utils

import scala.collection.mutable

case class MethodTableModel(moduleName: String, methodName: String, parentClassPath: String) {
  override def equals(other: Any): Boolean = {
    other match {
      case that: MethodTableModel => this.methodName == that.methodName
      case _                      => false
    }
  }

  override def hashCode: Int = {
    methodName.hashCode
  }
}

case class PackageContext(moduleName: String, packageTable: PackageTable)

class PackageTable() {

  private val methodTableSet = mutable.HashSet[MethodTableModel]()

  def add(moduleName: String, methodName: String, parentClassPath: String): Unit = {
    val packageMethod = MethodTableModel(moduleName, methodName, parentClassPath)
    if (!methodTableSet.contains(packageMethod)) {
      methodTableSet.add(packageMethod)
    }
  }

  def get(methodName: String): Set[MethodTableModel] = {
    methodTableSet.filter(method => method.methodName == methodName).toSet
  }

  def contains(methodName: String): Boolean = {
    methodTableSet.contains(MethodTableModel("", methodName, ""))
  }

  def getTable(): Set[MethodTableModel] = {
    methodTableSet.toSet
  }
}
