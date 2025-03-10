package io.shiftleft.resolver

import io.shiftleft.resolver.impl.BuildInfoExtractorMaven
import io.shiftleft.resolver.util.{PomContext, PomUtil}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.nio.file.{Files, Path}

class ParsePomXmlTests extends AnyWordSpec with Matchers {
  "test pom xml parsing" in {
    val xmlStr = Files.readString(Path.of("/home/ml/Downloads/commons-io-2.18.0.pom"))
    //val xmlStr = Files.readString(Path.of("/home/ml/Downloads/spring-boot-3.4.2.pom"))
    val xml = PomUtil.loadXml(xmlStr)
    println(PomUtil.extractContent(xml, PomContext.empty).coordinate.id.groupId)
    println(PomUtil.extractContent(xml, PomContext.empty).coordinate.id.artifactId)
    println(PomUtil.extractContent(xml, PomContext.empty).directDeps)
  }

}
