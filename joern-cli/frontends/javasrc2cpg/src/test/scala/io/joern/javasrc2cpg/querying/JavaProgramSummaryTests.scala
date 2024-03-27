package io.joern.javasrc2cpg.querying

import io.joern.javasrc2cpg.Config
import io.joern.javasrc2cpg.testfixtures.JavaSrcCode2CpgFixture
import io.joern.javasrc2cpg.typesolvers.JavaProgramSummary
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.utils.ProjectRoot
import org.scalatest.Inside.inside
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.util.Success

class JavaProgramSummaryTests extends AnyWordSpec with Matchers {

  "empty JSON object" should {
    "load properly" in {
      val json = "{}"
      inside(JavaProgramSummary.fromJsonString(json)) { case Success(mappings) =>
        mappings shouldBe empty
      }
    }
  }

  "namespace without types" should {
    "load properly" in {
      val json =
        """
          |{
          |  "Foo" : []
          |}
          |""".stripMargin
      inside(JavaProgramSummary.fromJsonString(json)) { case Success(mappings) =>
        mappings("Foo") shouldBe Set.empty
      }
    }
  }

  "namespace with single empty type" should {
    "load and render properly" in {
      val json =
        """
          |{
          | "org.codeminers": [
          |   {
          |     "name": "Foo",
          |     "methods": [],
          |     "fields": [],
          |     "isStatic": false,
          |     "innerClasses": []
          |   }
          | ]
          |}
          |""".stripMargin

      inside(JavaProgramSummary.fromJsonString(json)) { case Success(mappings) =>
        JavaProgramSummary(mappings :: Nil).asJavaCode shouldBe
          """
              |package org.codeminers;
              |
              |public class Foo{
              |
              |
              |
              |}
              |""".stripMargin :: Nil
      }
    }
  }

  "two namespaces with an empty type each" should {
    "load and render properly" in {
      val json =
        """
          |{
          | "org.codeminers1": [
          |   {
          |     "name": "Foo",
          |     "methods": [],
          |     "fields": [],
          |     "innerClasses": [],
          |     "isStatic": false
          |   }
          | ],
          | "org.codeminers2": [
          |   {
          |     "name": "Bar",
          |     "methods": [],
          |     "fields": [],
          |     "innerClasses": [],
          |     "isStatic": false
          |   }
          | ]
          |}
          |""".stripMargin

      inside(JavaProgramSummary.fromJsonString(json)) { case Success(mappings) =>
        JavaProgramSummary(mappings :: Nil).asJavaCode shouldBe List(
          """
            |package org.codeminers1;
            |
            |public class Foo{
            |
            |
            |
            |}
            |""".stripMargin,
          """
            |package org.codeminers2;
            |
            |public class Bar{
            |
            |
            |
            |}
            |""".stripMargin
        )
      }
    }
  }

  "two same-named namespaces with an empty type each" should {
    "be merged and rendered properly" in {
      val json1 =
        """
          |{
          | "org.codeminers": [
          |   {
          |     "name": "Foo",
          |     "methods": [],
          |     "fields": [],
          |     "innerClasses": [],
          |     "isStatic": false
          |   }
          | ]
          |}
          |""".stripMargin

      val json2 =
        """
          |{
          | "org.codeminers": [
          |   {
          |     "name": "Bar",
          |     "methods": [],
          |     "fields": [],
          |     "innerClasses": [],
          |     "isStatic": false
          |   }
          | ]
          |}
          |""".stripMargin

      val summary1 = JavaProgramSummary(JavaProgramSummary.fromJsonString(json1).toOption.toList)
      val summary2 = JavaProgramSummary(JavaProgramSummary.fromJsonString(json2).toOption.toList)
      inside((summary1 ++ summary2).asJavaCode.sorted) { case bar :: foo :: Nil =>
        bar shouldBe
          """
              |package org.codeminers;
              |
              |public class Bar{
              |
              |
              |
              |}
              |""".stripMargin
        foo shouldBe
          """
              |package org.codeminers;
              |
              |public class Foo{
              |
              |
              |
              |}
              |""".stripMargin
      }
    }
  }
}

class FileSystemJavaProgramSummaryTests extends JavaSrcCode2CpgFixture {

  private val config =
    Config().withTypeSummariesPath(
      ProjectRoot.relativise("joern-cli/frontends/javasrc2cpg/src/test/resources/programsummary_tests")
    )

  "okhttp3 newCall" should {
    val cpg = code("""
                     |import okhttp3.*;
                     |
                     |class Main {
                     | void run(Request req) {
                     |   var client = new OkHttpClient();
                     |   var call = client.newCall(req);
                     |   call.executeAsync();
                     | }
                     |}
                     |""".stripMargin).withConfig(config)

    "parameter `req` should have suggested type" in {
      inside(cpg.parameter("req").l) { case req :: Nil => req.typeFullName shouldBe "okhttp3.Request" }
    }

    "call `newCall` should have suggested type" in {
      inside(cpg.call("newCall").l) { case newCall :: Nil =>
        newCall.methodFullName shouldBe "okhttp3.OkHttpClient.newCall:okhttp3.Call(okhttp3.Request)"
        newCall.signature shouldBe "okhttp3.Call(okhttp3.Request)"
      }
    }

    "local `call` should have suggested type" in {
      inside(cpg.local("call").l) { case call :: Nil => call.typeFullName shouldBe "okhttp3.Call" }
    }
  }

  "okhttp3 Builder" should {
    val cpg = code("""
        |import okhttp3.*;
        |import okhttp3.OkHttpClient;
        |
        |public class Main {
        |  OkHttpClient getHttpClient() {
        |    var builder = new OkHttpClient.Builder();
        |    var client = builder.build();
        |    return client;
        |  }
        |  void run(Request req) {
        |    var call = getHttpClient().newCall(req);
        |    call.execute();
        |  }
        |}
        |""".stripMargin).withConfig(config)

    "parameter `req` should have suggested type" in {
      inside(cpg.parameter("req").l) { case req :: Nil => req.typeFullName shouldBe "okhttp3.Request" }
    }

    "call `newCall` should have suggested type" in {
      inside(cpg.call("newCall").l) { case newCall :: Nil =>
        newCall.methodFullName shouldBe "okhttp3.OkHttpClient.newCall:okhttp3.Call(okhttp3.Request)"
        newCall.signature shouldBe "okhttp3.Call(okhttp3.Request)"
      }
    }

    "call `build` should have suggested type" in {
      inside(cpg.call("build").l) { case build :: Nil =>
        build.methodFullName shouldBe "okhttp3.OkHttpClient$Builder.build:okhttp3.OkHttpClient()"
        build.signature shouldBe "okhttp3.OkHttpClient()"
      }
    }

    "local `call` should have suggested type" in {
      inside(cpg.local("call").l) { case call :: Nil => call.typeFullName shouldBe "okhttp3.Call" }
    }

    "local `builder` should have suggested type" in {
      inside(cpg.local("builder").l) { case builder :: Nil =>
        builder.typeFullName shouldBe "okhttp3.OkHttpClient$Builder"
      }
    }

    "local `client` should have suggested type" in {
      inside(cpg.local("client").l) { case client :: Nil => client.typeFullName shouldBe "okhttp3.OkHttpClient" }
    }

  }

}
