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
  val extensionInherits: ConcurrentHashMap[String, mutable.HashSet[String]] = new ConcurrentHashMap()

  /** Mapping from extension method fullName (provided by the compiler) to the fullName the frontend generates for
    * fullName uniqueness.
    */
  val extensionMethodFullNameMapping: ConcurrentHashMap[String, String] = new ConcurrentHashMap()

  /** Mapping from extension fullName to the members it defines as computed properties. */
  val extensionMembers: ConcurrentHashMap[String, mutable.ArrayBuffer[MemberInfo]] = new ConcurrentHashMap()

  def addExtensionMember(
    extensionFullName: String,
    memberName: String,
    memberCode: String,
    memberTypeFullName: String
  ): Unit = {
    extensionMembers.compute(
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
    extensionInherits.compute(
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

}
