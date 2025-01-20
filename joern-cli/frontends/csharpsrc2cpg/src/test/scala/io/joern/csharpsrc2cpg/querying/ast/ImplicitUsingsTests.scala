package io.joern.csharpsrc2cpg.querying.ast

import io.joern.csharpsrc2cpg.testfixtures.CSharpCode2CpgFixture
import io.shiftleft.semanticcpg.language.*

class ImplicitUsingsTests extends CSharpCode2CpgFixture {

  "top-level WriteLine call" when {

    "accompanied by a NET.Sdk csproj with ImplicitUsings set to `enable`" should {
      val cpg = code("""
          |Console.WriteLine("Foo");
          |""".stripMargin)
        .moreCode(
          """
            |<Project Sdk="Microsoft.NET.Sdk">
            | <PropertyGroup>
            |   <OutputType>Exe</OutputType>
            |   <ImplicitUsings>enable</ImplicitUsings>
            | </PropertyGroup>
            |</Project>
            |""".stripMargin,
          fileName = "App.csproj"
        )

      "resolve WriteLine call" in {
        cpg.call.nameExact("WriteLine").methodFullName.l shouldBe List(
          "System.Console.WriteLine:System.Void(System.String)"
        )
      }
    }

    "accompanied by a NET.Sdk csproj with ImplicitUsings set to `true`" should {
      val cpg = code("""
          |Console.WriteLine("Foo");
          |""".stripMargin)
        .moreCode(
          """
            |<Project Sdk="Microsoft.NET.Sdk">
            | <PropertyGroup>
            |   <OutputType>Exe</OutputType>
            |   <ImplicitUsings>true</ImplicitUsings>
            | </PropertyGroup>
            |</Project>
            |""".stripMargin,
          fileName = "App.csproj"
        )

      "resolve WriteLine call" in {
        cpg.call.nameExact("WriteLine").methodFullName.l shouldBe List(
          "System.Console.WriteLine:System.Void(System.String)"
        )
      }
    }

    "accompanied by a NET.Sdk csproj with ImplicitUsings omitted" should {
      val cpg = code("""
          |Console.WriteLine("Foo");
          |""".stripMargin)
        .moreCode(
          """
            |<Project Sdk="Microsoft.NET.Sdk">
            | <PropertyGroup>
            |   <OutputType>Exe</OutputType>
            | </PropertyGroup>
            |</Project>
            |""".stripMargin,
          fileName = "App.csproj"
        )

      "not resolve WriteLine call" in {
        cpg.call.nameExact("WriteLine").methodFullName.l shouldBe List(
          "<unresolvedNamespace>.WriteLine:<unresolvedSignature>"
        )
      }

    }

    "accompanied by a NET.Sdk csproj with ImplicitUsings set to `false`" should {
      val cpg = code("""
          |Console.WriteLine("Foo");
          |""".stripMargin)
        .moreCode(
          """
            |<Project Sdk="Microsoft.NET.Sdk">
            | <PropertyGroup>
            |   <OutputType>Exe</OutputType>
            |   <ImplicitUsings>false</ImplicitUsings>
            | </PropertyGroup>
            |</Project>
            |""".stripMargin,
          fileName = "App.csproj"
        )

      "not resolve WriteLine call" in {
        cpg.call.nameExact("WriteLine").methodFullName.l shouldBe List(
          "<unresolvedNamespace>.WriteLine:<unresolvedSignature>"
        )
      }
    }

    "accompanied by a NET.Sdk csproj with ImplicitUsings set to `disable`" should {
      val cpg = code("""
          |Console.WriteLine("Foo");
          |""".stripMargin)
        .moreCode(
          """
            |<Project Sdk="Microsoft.NET.Sdk">
            | <PropertyGroup>
            |   <OutputType>Exe</OutputType>
            |   <ImplicitUsings>disable</ImplicitUsings>
            | </PropertyGroup>
            |</Project>
            |""".stripMargin,
          fileName = "App.csproj"
        )

      "not resolve WriteLine call" in {
        cpg.call.nameExact("WriteLine").methodFullName.l shouldBe List(
          "<unresolvedNamespace>.WriteLine:<unresolvedSignature>"
        )
      }
    }

    "accompanied by a NET.Sdk csproj with ImplicitUsings enabled but excluding `System`" should {
      val cpg = code("""
          |Console.WriteLine("Foo");
          |""".stripMargin)
        .moreCode(
          """
            |<Project Sdk="Microsoft.NET.Sdk">
            | <PropertyGroup>
            |   <OutputType>Exe</OutputType>
            |   <ImplicitUsings>enable</ImplicitUsings>
            | </PropertyGroup>
            | <ItemGroup>
            |  <Using Remove="System" />
            | </ItemGroup>
            |</Project>
            |""".stripMargin,
          fileName = "App.csproj"
        )

      "not resolve WriteLine call" in {
        cpg.call.nameExact("WriteLine").methodFullName.l shouldBe List(
          "<unresolvedNamespace>.WriteLine:<unresolvedSignature>"
        )
      }
    }

    "accompanied by a NET.Sdk csproj with ImplicitUsings disabled but including `System`" should {
      val cpg = code("""
          |Console.WriteLine("Foo");
          |""".stripMargin)
        .moreCode(
          """
            |<Project Sdk="Microsoft.NET.Sdk">
            | <PropertyGroup>
            |   <OutputType>Exe</OutputType>
            |   <ImplicitUsings>false</ImplicitUsings>
            | </PropertyGroup>
            | <ItemGroup>
            |   <Using Include="System" />
            | </ItemGroup>
            |</Project>
            |""".stripMargin,
          fileName = "App.csproj"
        )

      "resolve WriteLine call" in {
        cpg.call.nameExact("WriteLine").methodFullName.l shouldBe List(
          "System.Console.WriteLine:System.Void(System.String)"
        )
      }
    }

    "accompanied by a NET.Sdk csproj with ImplicitUsings enabled but including and excluding `System`" should {
      val cpg = code("""
          |Console.WriteLine("Foo");
          |""".stripMargin)
        .moreCode(
          """
            |<Project Sdk="Microsoft.NET.Sdk">
            | <PropertyGroup>
            |   <OutputType>Exe</OutputType>
            |   <ImplicitUsings>true</ImplicitUsings>
            | </PropertyGroup>
            | <ItemGroup>
            |   <Using Include="System" />
            |   <Using Remove="System" />
            | </ItemGroup>
            |</Project>
            |""".stripMargin,
          fileName = "App.csproj"
        )

      "not resolve WriteLine call" in {
        cpg.call.nameExact("WriteLine").methodFullName.l shouldBe List(
          "<unresolvedNamespace>.WriteLine:<unresolvedSignature>"
        )
      }
    }

    "accompanied by a NET.Sdk csproj with ImplicitUsings enabled but excluding and including `System`" should {
      val cpg = code("""
          |Console.WriteLine("Foo");
          |""".stripMargin)
        .moreCode(
          """
            |<Project Sdk="Microsoft.NET.Sdk">
            | <PropertyGroup>
            |   <OutputType>Exe</OutputType>
            |   <ImplicitUsings>true</ImplicitUsings>
            | </PropertyGroup>
            | <ItemGroup>
            |   <Using Remove="System" />
            |   <Using Include="System" />
            | </ItemGroup>
            |</Project>
            |""".stripMargin,
          fileName = "App.csproj"
        )

      "resolve WriteLine call" in {
        cpg.call.nameExact("WriteLine").methodFullName.l shouldBe List(
          "System.Console.WriteLine:System.Void(System.String)"
        )
      }
    }
  }

}
