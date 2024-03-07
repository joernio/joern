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
        |      <PackageReference Include="CommandLineParser" Version="2.9.1" />
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
      "DotNetAstGen.csproj"
    )

    "result in downloading and summarizing the dependencies" in {
      val dd      = new DependencyDownloader(cpg, CSharpProgramSummary())
      val summary = dd.download()
    }

  }

}
