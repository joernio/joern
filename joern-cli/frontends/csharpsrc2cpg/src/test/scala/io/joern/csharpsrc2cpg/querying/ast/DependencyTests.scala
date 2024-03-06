package io.joern.csharpsrc2cpg.querying.ast

import io.joern.csharpsrc2cpg.testfixtures.CSharpCode2CpgFixture
import io.shiftleft.semanticcpg.language.*

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

}
