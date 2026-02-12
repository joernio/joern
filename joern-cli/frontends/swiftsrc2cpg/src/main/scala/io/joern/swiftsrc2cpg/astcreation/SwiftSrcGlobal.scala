package io.joern.swiftsrc2cpg.astcreation

import io.joern.x2cpg.datastructures.Global

import java.util.concurrent.ConcurrentHashMap
import scala.collection.mutable

object SwiftSrcGlobal {

  case class MemberInfo(name: String, code: String, typeFullName: String)

}

class SwiftSrcGlobal extends Global {

  import SwiftSrcGlobal.MemberInfo

  /** Mapping from extension fullName to the set of names it inherits from. */
  val extensionInheritMapping: ConcurrentHashMap[String, mutable.HashSet[String]] = new ConcurrentHashMap()

  /** Mapping from extension method fullName (provided by the compiler) to the fullName the frontend generates for
    * fullName uniqueness.
    */
  val extensionMethodFullNameMapping: ConcurrentHashMap[String, String] = new ConcurrentHashMap()

  /** Mapping from extension fullName to the members it defines as computed properties. */
  val extensionMemberMapping: ConcurrentHashMap[String, mutable.ArrayBuffer[MemberInfo]] = new ConcurrentHashMap()

  /** Mapping from member fullName to method fullName from its computed property. */
  val memberPropertyMapping: ConcurrentHashMap[String, String] = new ConcurrentHashMap()

  def addExtensionMember(
    extensionFullName: String,
    memberName: String,
    memberCode: String,
    memberTypeFullName: String
  ): Unit = {
    extensionMemberMapping.compute(
      extensionFullName,
      (_, previousList) => {
        val memberInfo = MemberInfo(memberName, memberCode, memberTypeFullName)
        if (previousList == null) {
          mutable.ArrayBuffer(memberInfo)
        } else {
          previousList.append(memberInfo)
        }
      }
    )
  }

  def addExtensionInherits(extensionFullName: String, inheritNames: Seq[String]): Unit = {
    extensionInheritMapping.compute(
      extensionFullName,
      (_, previousSet) => {
        if (previousSet == null) {
          mutable.HashSet.from(inheritNames)
        } else {
          previousSet.addAll(inheritNames)
        }
      }
    )
  }

  def addExtensionMethodFullName(extensionMethodFullName: String, fullName: String): Unit = {
    extensionMethodFullNameMapping.putIfAbsent(extensionMethodFullName, fullName)
  }

  def addMemberPropertyFullName(memberFullName: String, propertyFullName: String): Unit = {
    memberPropertyMapping.putIfAbsent(memberFullName, propertyFullName)
  }

}
