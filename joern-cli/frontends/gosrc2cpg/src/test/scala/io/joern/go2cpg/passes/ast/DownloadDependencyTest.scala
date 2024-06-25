package io.joern.go2cpg.passes.ast

import io.joern.go2cpg.testfixtures.GoCodeToCpgSuite
import io.joern.gosrc2cpg.Config
import io.joern.gosrc2cpg.astcreation.Defines
import io.joern.gosrc2cpg.datastructures.{GoGlobal, LambdaTypeInfo, MethodCacheMetaData, NameSpaceMetaData}
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.semanticcpg.language.*

import scala.jdk.CollectionConverters.*

class DownloadDependencyTest extends GoCodeToCpgSuite {
  val IGNORE_TEST_FILE_REGEX = ".*_test(s)?.*"
  "Simple use case of third-party dependency download" should {
    val config = Config().withFetchDependencies(true).withIgnoredFilesRegex(IGNORE_TEST_FILE_REGEX)
    val cpg = code(
      """
        |module joern.io/sample
        |go 1.18
        |require (
        |	github.com/google/uuid v1.3.1
        |)
        |""".stripMargin,
      "go.mod"
    ).moreCode("""
        |package main
        |import "github.com/google/uuid"
        |func main()  {
        |  var uud = uuid.NewString()
        |}
        |""".stripMargin)
      .withConfig(config)

    "Check CALL Node" in {
      val List(x) = cpg.call("NewString").l
      x.typeFullName shouldBe "string"
    }
  }

  // NOTE: With respect to conversation on this PR - https://github.com/joernio/joern/pull/3753
  // ignoring the below uni tests, which tries to download the dependencies.
  "Download dependency example with different package and namespace name" ignore {
    val config = Config().withFetchDependencies(true).withIgnoredFilesRegex(IGNORE_TEST_FILE_REGEX)
    val cpg = code(
      """
        |module joern.io/sample
        |go 1.18
        |require (
        |	github.com/aerospike/aerospike-client-go/v6 v6.14.0
        |)
        |""".stripMargin,
      "go.mod"
    ).moreCode("""
          |package main
          |import "github.com/aerospike/aerospike-client-go/v6"
          |func main()  {
          |  client, err := aerospike.NewClient("localhost", 3000)
          |  var test = aerospike.UserAdmin
          |}
          |""".stripMargin)
      .withConfig(config)

    "Check CALL Node" in {
      val List(x) = cpg.call("NewClient").l
      x.typeFullName shouldBe "*github.com/aerospike/aerospike-client-go/v6.Client"
    }

    "Check if we are able to identify the type of constants accessible out side dependencies code" in {
      val List(t) = cpg.local("test").l
      t.typeFullName shouldBe "github.com/aerospike/aerospike-client-go/v6.privilegeCode"
    }
  }

  "unresolved dependency tests one" should {
    val cpg = code(
      """
        |module joern.io/sample
        |go 1.18
        |require (
        |	joern.io/sampletwo v1.3.1
        |)
        |""".stripMargin,
      "go.mod"
    ).moreCode("""
          |package main
          |import "joern.io/sampletwo"
          |func main()  {
          |  var a = sampletwo.Person{Name:"Pandurang"}
          |  var b = a.Name
          |  var c = a.FullName()
          |  var d = a.Process().FullName()
          |  var e = a.Process().SomeField
          |}
          |""".stripMargin)

    "Be correct for CALL Node typeFullNames" in {
      val List(a, b, c, d, e, f, g) = cpg.call.nameNot(Operators.assignment).l
      a.typeFullName shouldBe "joern.io/sampletwo.Person"
      b.typeFullName shouldBe "joern.io/sampletwo.Person.Name.<FieldAccess>.<unknown>"
      c.typeFullName shouldBe "joern.io/sampletwo.Person.FullName.<ReturnType>.<unknown>"
      d.typeFullName shouldBe "joern.io/sampletwo.Person.Process.<ReturnType>.<unknown>.FullName.<ReturnType>.<unknown>"
      e.typeFullName shouldBe "joern.io/sampletwo.Person.Process.<ReturnType>.<unknown>"
      f.typeFullName shouldBe "joern.io/sampletwo.Person.Process.<ReturnType>.<unknown>.SomeField.<FieldAccess>.<unknown>"
    }
  }

  "unresolved dependency tests two" should {
    val cpg = code("""
        |package main
        |import (
        |    "github.com/rs/zerolog"
        |    "github.com/rs/zerolog/log"
        |)
        |func main() {
        |    zerolog.SetGlobalLevel(zerolog.InfoLevel)
        |    log.Error().Msg("Error message")
        |    log.Warn().Msg("Warning message")
        |}
        |""".stripMargin)

    "Be correct for CALL Node typeFullNames" in {
      val List(a, b, c, d, e) = cpg.call.nameNot(Operators.fieldAccess).l
      a.typeFullName shouldBe "github.com/rs/zerolog.SetGlobalLevel.<ReturnType>.<unknown>"
      b.typeFullName shouldBe "github.com/rs/zerolog/log.Error.<ReturnType>.<unknown>.Msg.<ReturnType>.<unknown>"
      c.typeFullName shouldBe "github.com/rs/zerolog/log.Error.<ReturnType>.<unknown>"
      d.typeFullName shouldBe "github.com/rs/zerolog/log.Warn.<ReturnType>.<unknown>.Msg.<ReturnType>.<unknown>"
      e.typeFullName shouldBe "github.com/rs/zerolog/log.Warn.<ReturnType>.<unknown>"
    }
  }

  // NOTE: With respect to conversation on this PR - https://github.com/joernio/joern/pull/3753
  // ignoring the below uni tests, which tries to download the dependencies.
  "dependency resolution having type struct" ignore {
    val config = Config().withFetchDependencies(true).withIgnoredFilesRegex(IGNORE_TEST_FILE_REGEX)
    val cpg = code(
      """
        |module joern.io/sample
        |go 1.18
        |
        |require (
        | github.com/redis/go-redis/v9 v9.2.1
        |)
        |""".stripMargin,
      "go.mod"
    ).moreCode("""
          |package main
          |import "github.com/redis/go-redis/v9"
          |
          |type Client struct {
          |	rdb redis.UniversalClient
          |}
          |
          |func (c *Client) setValue() {
          | key := "key"
          | value := "value"
          | err := c.rdb.Close()
          |}
          |""".stripMargin)
      .withConfig(config)

    "Test basic ast structure" in {
      val List(typeDeclNode) = cpg.typeDecl.nameExact("Client").l
      typeDeclNode.fullName shouldBe "main.Client"
      typeDeclNode.member.size shouldBe 1
      typeDeclNode.member.head.typeFullName shouldBe "github.com/redis/go-redis/v9.UniversalClient"
    }

    "Test call node" ignore {
      // TODO: Need to handle interface Type for caching the meta data to make this test work.
      val List(callNode) = cpg.call.name("Close").l
      callNode.typeFullName shouldBe "error"
      callNode.methodFullName shouldBe "github.com/redis/go-redis/v9.UnversalClient.Close"
    }
  }

  "If the dependency is not getting used then it " should {
    val goGlobal = GoGlobal()
    val config   = Config().withFetchDependencies(true).withIgnoredFilesRegex(IGNORE_TEST_FILE_REGEX)
    val cpg = code(
      """
        |module joern.io/sample
        |go 1.18
        |
        |require (
        | github.com/rs/zerolog v1.31.0
        |)
        |""".stripMargin,
      "go.mod"
    ).moreCode("""
          |package main
          |func main()  {
          |}
          |""".stripMargin)
      .withConfig(config)
      .withGoGlobal(goGlobal)

    // Dummy cpg query which will initiate CPG creation.
    cpg.method.l

    "not be downloaded " in {
      val goModHelper  = cpg.getModHelper()
      val dependencies = goModHelper.getModMetaData().get.dependencies
      dependencies.size shouldBe 1
      val List(dep) = dependencies
      dep.module shouldBe "github.com/rs/zerolog"
      dep.beingUsed shouldBe false
    }

    "not create any entry in package to namespace mapping" in {
      // it should not add `main` in the mapping as well as it should not contain any dependency mapping in the case current sample
      goGlobal.aliasToNameSpaceMapping.size() shouldBe 0

    }

    "not create any entry in lambda signature to return type mapping" in {
      // "github.com/rs/zerolog" dependency has lambda Struct Types declared in the code. However they should not get cached as they are not getting used anywhere.
      goGlobal.lambdaSignatureToLambdaTypeMap.size() shouldBe 0
    }

    "not create any entry in package level ctor map" in {
      // This anyway should only be populated for main source code.
      goGlobal.pkgLevelVarAndConstantAstMap.size() shouldBe 0
    }

    "not create any entry in method full name to return type map" in {
      // This should only contain the `main` method return type mapping as main source code is not invoking any of the dependency method.
      goGlobal.nameSpaceMetaDataMap.size() shouldBe 1
      val Array(metadata) = goGlobal.nameSpaceMetaDataMap.values().iterator().toArray
      metadata.methodMetaMap.size() shouldBe 1
      val List(mainfullname) = metadata.methodMetaMap.keys().asIterator().toList
      mainfullname shouldBe "main"
      val Array(returnType) = metadata.methodMetaMap.values().toArray
      returnType shouldBe MethodCacheMetaData(Defines.voidTypeName, "main.main()")
    }

    "not create any entry in struct member to type map" in {
      // This should be empty as neither main code has defined any struct type nor we are accessing the third party struct type.
      goGlobal.nameSpaceMetaDataMap.size() shouldBe 1
      val Array(metadata) = goGlobal.nameSpaceMetaDataMap.values().iterator().toArray
      metadata.structTypeMembers.size() shouldBe 0
    }
  }

  "The dependency is getting imported somewhere but not getting used then it" should {
    val goGlobal = GoGlobal()
    val config   = Config().withFetchDependencies(true).withIgnoredFilesRegex(IGNORE_TEST_FILE_REGEX)
    val cpg = code(
      """
        |module joern.io/sample
        |go 1.18
        |
        |require (
        | github.com/rs/zerolog v1.31.0
        | github.com/google/uuid v1.3.1
        |)
        |""".stripMargin,
      "go.mod"
    ).moreCode("""
          |package main
          |import "github.com/rs/zerolog"
          |func main()  {
          |}
          |""".stripMargin)
      .withConfig(config)
      .withGoGlobal(goGlobal)

    // Dummy cpg query which will initiate CPG creation.
    cpg.method.l

    "download the dependency" in {
      val goModHelper  = cpg.getModHelper()
      val dependencies = goModHelper.getModMetaData().get.dependencies
      dependencies.size shouldBe 2
      val List(depone, deptwo) = dependencies
      depone.module shouldBe "github.com/rs/zerolog"
      depone.beingUsed shouldBe true

      deptwo.module shouldBe "github.com/google/uuid"
      deptwo.beingUsed shouldBe false
    }

    "not create any entry in package to namespace mapping" in {
      // it should not add `main` in the mapping as well as it should not contain any dependency mapping
      goGlobal.aliasToNameSpaceMapping.size() shouldBe 0
    }

    "not create any entry in lambda signature to return type mapping" in {
      // "github.com/rs/zerolog" dependency has lambda Struct Types declared in the code. However they should not get cached as they are not getting used anywhere.
      goGlobal.lambdaSignatureToLambdaTypeMap.size() shouldBe 0
    }

    "not create any entry in package level ctor map" in {
      // This anyway should only be populated for main source code.
      goGlobal.pkgLevelVarAndConstantAstMap.size() shouldBe 0
    }

    // TODO: Need to update these tests with some more improvements
    "not create any entry in method full name to return type map" ignore {
      // This should only contain the `main` method return type mapping as main source code is not invoking any of the dependency method.
      goGlobal.nameSpaceMetaDataMap.size() shouldBe 1
      val Array(metadata) = goGlobal.nameSpaceMetaDataMap.values().iterator().toArray
      metadata.methodMetaMap.size() shouldBe 1
      val List(mainfullname) = metadata.methodMetaMap.keys().asIterator().toList
      mainfullname shouldBe "main"
      val Array(returnType) = metadata.methodMetaMap.values().toArray
      returnType shouldBe MethodCacheMetaData(Defines.voidTypeName, "main.main()")
    }

    // TODO: Need to update these tests with some more improvements
    "not create any entry in struct member to type map" ignore {
      // This should be empty as neither main code has defined any struct type nor we are accessing the third party struct type.
      goGlobal.nameSpaceMetaDataMap.size() shouldBe 1
      val Array(metadata) = goGlobal.nameSpaceMetaDataMap.values().iterator().toArray
      metadata.structTypeMembers.size() shouldBe 0
    }
  }

  "The dependency is getting imported and used in the code then it" should {
    val goGlobal = GoGlobal()
    val config   = Config().withFetchDependencies(true).withIgnoredFilesRegex(IGNORE_TEST_FILE_REGEX)
    val cpg = code(
      """
        |module joern.io/sample
        |go 1.18
        |
        |require (
        | github.com/rs/zerolog v1.31.0
        | github.com/google/uuid v1.3.1
        |)
        |""".stripMargin,
      "go.mod"
    ).moreCode("""
          |package main
          |import (
          |    "github.com/rs/zerolog"
          |    "github.com/rs/zerolog/log"
          |)
          |func main() {
          |    var eventHandler = func(e *zerolog.Event, level zerolog.Level, message string){
          |    }
          |    zerolog.SetGlobalLevel(zerolog.InfoLevel)
          |    log.Error().Msg("Error message")
          |    log.Warn().Msg("Warning message")
          |}
          |""".stripMargin)
      .withConfig(config)
      .withGoGlobal(goGlobal)

    // Dummy cpg query which will initiate CPG creation.
    cpg.method.l

    "Be correct for CALL Node typeFullNames" in {
      val List(a, b, c, d, e) =
        cpg.call.where(_.and(_.nameNot(Operators.fieldAccess), _.nameNot(Operators.assignment))).l
      a.typeFullName shouldBe "void"
      b.typeFullName shouldBe "void"
      c.typeFullName shouldBe "*github.com/rs/zerolog.Event"
      d.typeFullName shouldBe "void"
      e.typeFullName shouldBe "*github.com/rs/zerolog.Event"
    }

    "download the dependency" in {
      val goModHelper  = cpg.getModHelper()
      val dependencies = goModHelper.getModMetaData().get.dependencies
      dependencies.size shouldBe 2
      val List(depone, deptwo) = dependencies
      depone.module shouldBe "github.com/rs/zerolog"
      depone.beingUsed shouldBe true

      deptwo.module shouldBe "github.com/google/uuid"
      deptwo.beingUsed shouldBe false
    }

    "not create any entry in package to namespace mapping" in {
      // it should not add `main` in the mapping as well as it should not contain any dependency mapping unless the folder name and package name is different.
      goGlobal.aliasToNameSpaceMapping.size() shouldBe 0
    }

    "not create any entry in lambda signature to return type mapping" in {
      // "github.com/rs/zerolog" dependency has lambda Struct Types declared in the code. However they should not get cached as they are not getting used anywhere.
      goGlobal.lambdaSignatureToLambdaTypeMap.size() shouldBe 1
      goGlobal.lambdaSignatureToLambdaTypeMap
        .values()
        .toArray()
        .map(_.asInstanceOf[java.util.Set[LambdaTypeInfo]].asScala)
        .flatMap(_.toList) shouldBe Array(LambdaTypeInfo("github.com/rs/zerolog.HookFunc", "void"))
    }

    "not create any entry in package level ctor map" in {
      // This anyway should only be populated for main source code.
      goGlobal.pkgLevelVarAndConstantAstMap.size() shouldBe 0
    }

    // TODO: Need to update these tests with some more improvements
    "not create any entry in method full name to return type map" ignore {
      // This should only contain the `main` method return type mapping as main source code is not invoking any of the dependency method.
      // TODO: While doing the implementation we need update this test
      // Lambda expression return types are also getting recorded under this map
      goGlobal.nameSpaceMetaDataMap.size() shouldBe 1
      val Array(metadata) = goGlobal.nameSpaceMetaDataMap.values().iterator().toArray
      metadata.methodMetaMap.size() shouldBe 1
      val List(mainfullname) = metadata.methodMetaMap.keys().asIterator().toList
      mainfullname shouldBe "main"
      val Array(returnType) = metadata.methodMetaMap.values().toArray
      returnType shouldBe MethodCacheMetaData(Defines.voidTypeName, "main.main()")
    }

    // TODO: Need to update these tests with some more improvements
    "not create any entry in struct member to type map" ignore {
      // TODO: This test might require to update when we implement
      // 1. Struct Type is directly being used
      // 2. Struct Type is being passed as parameter or returned as value of method that is being used.
      // 3. A method of Struct Type being used.
      goGlobal.nameSpaceMetaDataMap.size() shouldBe 1
      val Array(metadata) = goGlobal.nameSpaceMetaDataMap.values().iterator().toArray
      metadata.structTypeMembers.size() shouldBe 0
    }
  }
}

// TODO: Add unit tests with imports having builtin packages.
