package io.joern.dataflowengineoss.queryengine

import io.joern.dataflowengineoss.language._
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.{CfgNode, StoredNode}
import io.shiftleft.semanticcpg.language._
import overflowdb.traversal.Traversal

import java.util.Calendar
import scala.collection.mutable.ListBuffer

object TestFlows extends App {
  implicit val engineContext: EngineContext =
    EngineContext(config = EngineConfig(4))

  val cpg = Cpg.withStorage("/Users/rahul/IdeaProjects/cpgs/privado-accounts-api-joern-1.1.1309.cpg.bin")//562
  //val cpg = Cpg.withStorage("/Users/rahul/IdeaProjects/cpgs/opencrx-joern-1.1.1309.cpg.bin")//961
  //val cpg = Cpg.withStorage("/Users/rahul/IdeaProjects/cpgs/snap.cpg.bin")
  //val cpg = Cpg.withStorage("/Users/rahul/IdeaProjects/cpgs/shopizer-joern-1.1.1309.cpg.bin")
  //val cpg = Cpg.withStorage("/Users/rahul/IdeaProjects/cpgs/E-Commerce-Resturant-Android-Java-Apps-joern-1.1.1338.cpg.bin") // 1027 1027
  //val cpg = Cpg.withStorage("/Users/rahul/IdeaProjects/cpgs/alovoa-joern-1.1.1309.cpg.bin") //5577
  //val cpg = Cpg.withStorage("/Users/rahul/IdeaProjects/cpgs/clevertap-android-sdk-joern-1.1.1338.cpg.bin")
  //val cpg = Cpg.withStorage("/Users/rahul/IdeaProjects/cpgs/plaid-java-joern-1.1.1338.cpg.bin")

  //val cpg = Cpg.withStorage("/Users/rahul/IdeaProjects/cpgs/sms-backup-plus.cpg.bin")// 215
  //val cpg = Cpg.withStorage("/Users/rahul/IdeaProjects/cpgs/spring-boot-student-joern-1.1.1338.cpg.bin")//6033
  //val cpg = Cpg.withStorage("/Users/rahul/IdeaProjects/cpgs/sms-backup-plus.cpg.bin")
  //val cpg = Cpg.withStorage("/Users/rahul/IdeaProjects/cpgs/leaky-java-app.cpg.bin")

  val sinks   = getSinks
  val sources = getSources
  println(s" ${Calendar.getInstance().getTime} started finding flows ..")
  val paths = sinks.reachableByFlows(sources).l
  val outPutPath = ListBuffer[List[String]]()
  val outputPathIds = ListBuffer[String]()
  paths.foreach(path => {
    val pathIds = ListBuffer[String]()
    val someNewList = ListBuffer[String]()
    path.elements.foreach(element => {
      pathIds += s"${element.id()}"
      someNewList.addOne(s"${element.id()} - ${element.code} - ${element.file.name.headOption.getOrElse("<empty>")} - ${element.lineNumber.getOrElse("NON")}")
    })
    outputPathIds.addOne(pathIds.toList.mkString("->"))
    outPutPath.addOne(someNewList.toList)
  })
  for (i <- 0 to outPutPath.size - 1) {
    println(outputPathIds(i))
    outPutPath(i).foreach(step => {
      println(step)
    })
    println("\n\n\n --- ")

  }

  println(s" ${Calendar.getInstance().getTime}its done. size - ${paths.size}")

  def getSources: List[CfgNode] = {
    def filterSources(traversal: Traversal[StoredNode]) = {
      traversal.tag
        .nameExact("catLevelOne")
        .or(_.valueExact("sources"), _.valueExact("DerivedSources"))
    }
    cpg.literal
      .where(filterSources)
      .l ++ cpg.identifier
      .where(filterSources)
      .l ++ cpg.call
      .where(filterSources)
      .l ++ cpg.argument.isFieldIdentifier.where(filterSources).l

  }

  def getSinks: List[CfgNode] = {
    cpg.call.where(_.tag.nameExact("catLevelOne").valueExact("sinks")).l
  }
  }
