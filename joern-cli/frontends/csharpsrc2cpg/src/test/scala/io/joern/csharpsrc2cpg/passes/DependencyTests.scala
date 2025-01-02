package io.joern.csharpsrc2cpg.passes

import io.joern.csharpsrc2cpg.Config
import io.joern.csharpsrc2cpg.testfixtures.CSharpCode2CpgFixture
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.utils.ProjectRoot

class DependencyTests extends CSharpCode2CpgFixture {

  "a `csproj` file" should {

    val cpg = code(
      """
        |<Project Sdk="Microsoft.NET.Sdk">
        |
        |    <PropertyGroup>
        |        <Version Condition=" '$(NEXT_VERSION)' == '' ">0.0.1-local</Version>
        |        <Version Condition=" '$(NEXT_VERSION)' != '' ">$(NEXT_VERSION)</Version>
        |        <OutputType>Exe</OutputType>
        |        <TargetFramework>net8.0</TargetFramework>
        |        <ImplicitUsings>enable</ImplicitUsings>
        |        <Nullable>enable</Nullable>
        |        <SelfContained>true</SelfContained>
        |        <PublishSingleFile>true</PublishSingleFile>
        |        <EnableCompressionInSingleFile>true</EnableCompressionInSingleFile>
        |        <InvariantGlobalization>true</InvariantGlobalization>
        |    </PropertyGroup>
        |
        |    <ItemGroup>
        |      <PackageReference Include="CommandLineParser" Version="2.9.1" />
        |      <PackageReference Include="Microsoft.AspNetCore.Mvc.NewtonsoftJson" Version="3.0.0" />
        |      <PackageReference Include="Microsoft.CodeAnalysis" Version="4.8.0" />
        |      <PackageReference Include="Mono.Cecil" Version="0.11.5" />
        |      <PackageReference Include="Newtonsoft.Json" Version="13.0.3" />
        |    </ItemGroup>
        |
        |    <ItemGroup>
        |      <Folder Include="release\" />
        |    </ItemGroup>
        |
        |</Project>
        |
        |""".stripMargin,
      "DotNetAstGen.csproj"
    )

    "have the package references persisted with versions" in {
      inside(cpg.dependency.l) {
        case commandLineParser :: newtonsoftJson :: codeAnalysis :: cecil :: json :: Nil =>
          commandLineParser.name shouldBe "CommandLineParser"
          commandLineParser.version shouldBe "2.9.1"

          newtonsoftJson.name shouldBe "Microsoft.AspNetCore.Mvc.NewtonsoftJson"
          newtonsoftJson.version shouldBe "3.0.0"

          codeAnalysis.name shouldBe "Microsoft.CodeAnalysis"
          codeAnalysis.version shouldBe "4.8.0"

          cecil.name shouldBe "Mono.Cecil"
          cecil.version shouldBe "0.11.5"

          json.name shouldBe "Newtonsoft.Json"
          json.version shouldBe "13.0.3"
        case xs => fail(s"Expected exactly 5 dependencies, instead got [${xs.name.mkString(",")}]")
      }
    }

  }

  "a `csproj` file specifying a builtin dependency" should {

    val cpg = code("""
        |using System;
        |using Microsoft.EntityFrameworkCore;
        |
        |public class Foo {
        |
        | static void bar(ModelBuilder modelBuilder)
        | {
        |   modelBuilder.Entity("test");
        | }
        |
        |}
        |""".stripMargin)
      .moreCode(
        """
        |<Project Sdk="Microsoft.NET.Sdk">
        |    <ItemGroup>
        |      <PackageReference Include="Microsoft.EntityFrameworkCore" Version="doesNotExist" />
        |    </ItemGroup>
        |</Project>
        |""".stripMargin,
        "Foo.csproj"
      )
      .withConfig(Config().withDownloadDependencies(true))

    "resolve the call from the local builtins, as the specified dependency would not successfully download" in {
      inside(cpg.call("Entity").headOption) {
        case Some(entity) =>
          entity.methodFullName shouldBe "Microsoft.EntityFrameworkCore.ModelBuilder.Entity:Microsoft.EntityFrameworkCore.Metadata.Builders.EntityTypeBuilder(System.String)"
        case None =>
          fail("Expected a call node for `Entity`")
      }
    }
  }

  "a `csproj` file specifying a built-in dependency but built-in type summaries are disabled" when {
    val csCode = """
                   |using Microsoft.EntityFrameworkCore;
                   |
                   |public class Foo
                   |{
                   | static void bar(ModelBuilder modelBuilder)
                   | {
                   |   modelBuilder.Entity("test");
                   | }
                   |}""".stripMargin
    val csProj = """
                   |<Project Sdk="Microsoft.NET.Sdk">
                   | <ItemGroup>
                   |   <PackageReference Include="Microsoft.EntityFrameworkCore" Version="6.0.36" />
                   | </ItemGroup>
                   |</Project>
                   |""".stripMargin

    "the ability to download dependencies is also turned off" should {
      val cpg = code(csCode)
        .moreCode(csProj, "Foo.csproj")
        .withConfig(Config().withUseBuiltinSummaries(false).withDownloadDependencies(false))
      "not resolve the call since there are no type summaries available for it" in {
        inside(cpg.call("Entity").headOption) {
          case Some(entity) => entity.methodFullName shouldBe "ModelBuilder.Entity:<unresolvedSignature>"
          case None         => fail("Expected call node for `Entity`")
        }
      }
    }

    "the ability to download dependencies is turned on" should {
      val cpg = code(csCode)
        .moreCode(csProj, "Foo.csproj")
        .withConfig(Config().withUseBuiltinSummaries(false).withDownloadDependencies(true))
      "resolve the call since the dependency shall be downloaded and a type summary for it be built" in {
        inside(cpg.call("Entity").headOption) {
          case Some(entity) =>
            entity.methodFullName shouldBe "Microsoft.EntityFrameworkCore.ModelBuilder.Entity:Microsoft.EntityFrameworkCore.Metadata.Builders.EntityTypeBuilder(System.String)"
          case None => fail("Expected call node for `Entity`")
        }
      }
    }

    "download dependencies is disabled but external-summary-paths is pointing to the built-in directory" should {
      val externalSummaryPaths =
        Set(ProjectRoot.relativise("joern-cli/frontends/csharpsrc2cpg/src/main/resources/builtin_types"))
      val cpg = code(csCode)
        .moreCode(csProj, "Foo.csproj")
        .withConfig(
          Config()
            .withDownloadDependencies(false)
            .withUseBuiltinSummaries(false)
            .withExternalSummaryPaths(externalSummaryPaths)
        )

      "resolve the call since its summary can be found in the provided directory" in {
        inside(cpg.call("Entity").headOption) {
          case Some(entity) =>
            entity.methodFullName shouldBe "Microsoft.EntityFrameworkCore.ModelBuilder.Entity:Microsoft.EntityFrameworkCore.Metadata.Builders.EntityTypeBuilder(System.String)"
          case None => fail("Expected call node for `Entity`")
        }
      }

    }
  }

  "a `csproj` file specifying a dependency with the `Update` attribute" should {

    val cpg = code("""
        |using System;
        |using Microsoft.EntityFrameworkCore;
        |
        |public class Foo {
        |
        | static void bar(ModelBuilder modelBuilder)
        | {
        |   modelBuilder.Entity("test");
        | }
        |
        |}
        |""".stripMargin)
      .moreCode(
        """
          |<Project Sdk="Microsoft.NET.Sdk">
          |    <ItemGroup>
          |      <PackageReference Update="coverlet.msbuild" Version="3.2.0"/>
          |    </ItemGroup>
          |</Project>
          |""".stripMargin,
        "Foo.csproj"
      )

    "have the package references persisted with versions" in {
      inside(cpg.dependency.l) {
        case msbuild :: Nil =>
          msbuild.name shouldBe "coverlet.msbuild"
          msbuild.version shouldBe "3.2.0"
        case xs => fail(s"Expected exactly 1 dependencies, instead got [${xs.name.mkString(",")}]")
      }
    }

  }

  "a csproj file specifying a dependency with trailing or leading whitespaces" should {
    val config = Config().withDownloadDependencies(true);
    "not throw a exception" in {
      val cpg = code("""
          |namespace Foo;
          |""".stripMargin)
        .moreCode(
          """
            |<Project Sdk="Microsoft.NET.Sdk">
            |    <ItemGroup>
            |      <PackageReference Include=" System.Security.Cryptography.Pkcs" Version="6.0.4"/>
            |    </ItemGroup>
            |</Project>
            |""".stripMargin,
          "Foo.csproj"
        )
        .withConfig(config)

      inside(cpg.dependency.l) { case dep :: Nil =>
        dep.name shouldBe " System.Security.Cryptography.Pkcs"
        dep.version shouldBe "6.0.4"
      }
    }
  }
}
