package io.joern.javasrc2cpg.querying

import io.joern.javasrc2cpg.testfixtures.JavaSrcCode2CpgFixture
import io.joern.x2cpg.utils.ArtifactFetcherTests.artifactFetcherTest
import io.joern.x2cpg.utils.RetryableAssertion.eventually
import io.joern.x2cpg.utils.{HttpArtifact, NetworkTest}
import io.shiftleft.semanticcpg.language.*
import org.scalatest.Assertion

import java.io.IOException

class RemoteDependencyTests extends JavaSrcCode2CpgFixture {

  "code that relies on a downloaded dependency" should {
    "resolve the type from  the dependency" taggedAs NetworkTest in {
      eventually[Assertion, IOException] {
        val cpg = code("""
            |import java.util.*;
            |import org.apache.commons.collections4.bag.*;
            |
            |public class Test {
            |  void foo() {
            |    HashBag b = new HashBag();
            |  }
            |}
            |""".stripMargin).withRemoteInferenceJars(
          HttpArtifact(
            "https://repo1.maven.org/maven2/org/apache/commons/commons-collections4/4.5.0/commons-collections4-4.5.0.jar",
            "00f93263c267be201b8ae521b44a7137271b16688435340bf629db1bac0a5845"
          )
        )
        cpg.method.name("foo").ast.isLocal.name("b").typeFullName.l shouldBe List(
          "org.apache.commons.collections4.bag.HashBag"
        )
      }
    }
  }

}
