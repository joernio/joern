package io.joern.csharpsrc2cpg.io

import io.joern.csharpsrc2cpg.Config
import io.joern.csharpsrc2cpg.datastructures.CSharpProgramSummary
import io.joern.csharpsrc2cpg.testfixtures.CSharpCode2CpgFixture
import io.joern.csharpsrc2cpg.utils.DependencyDownloader

class DependencyDownloadTests extends CSharpCode2CpgFixture {

  "parsing and downloading dependencies" should {

    val cpg = code(
      """
        |<Project Sdk="Microsoft.NET.Sdk">
        |
        |    <ItemGroup>
        |      <PackageReference Include="CommandLineParser" />
        |      <PackageReference Include="Microsoft.AspNetCore.Mvc.NewtonsoftJson" Version="3.0.0" />
        |    </ItemGroup>
        |
        |    <ItemGroup>
        |      <Folder Include="release\" />
        |    </ItemGroup>
        |
        |</Project>
        |
        |""".stripMargin,
      "dotnetastgen-macos.csproj"
    )

    val dd      = new DependencyDownloader(cpg, Config(), CSharpProgramSummary())
    val summary = dd.download()

    "summarize the latest version of CommandLineParser (as it publishes with PDB files)" in {
      summary.typesUnderNamespace("CommandLine.Core") should not be empty
      summary.typesUnderNamespace("CommandLine.Infrastructure") should not be empty
      summary.typesUnderNamespace("CommandLine.Text") should not be empty
    }

    "summarize CommandLineParser's transient dependencies" in {
      summary.typesUnderNamespace("RailwaySharp.ErrorHandling") should not be empty
      summary.typesUnderNamespace("CSharpx") should not be empty
    }

    "manage to summarize NewtonsoftJson (as it publishes without PDB files, but DotNetAstGen can generate these)" in {
      summary.typesUnderNamespace("Microsoft.AspNetCore.Mvc.NewtonsoftJson") should not be empty
    }

  }

}
