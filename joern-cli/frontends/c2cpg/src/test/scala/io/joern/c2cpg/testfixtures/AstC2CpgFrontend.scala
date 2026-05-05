package io.joern.c2cpg.testfixtures

import io.joern.c2cpg.C2Cpg.DefaultIgnoredFolders
import io.joern.c2cpg.Config
import io.joern.c2cpg.parser.FileDefaults
import io.joern.c2cpg.passes.{AstCreationPass, FunctionDeclNodePass}
import io.joern.x2cpg.{SourceFiles, ValidationMode}
import io.joern.x2cpg.X2Cpg.newEmptyCpg
import io.joern.x2cpg.testfixtures.LanguageFrontend
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.semanticcpg.validation.{PostFrontendValidator, ValidationLevel}

trait AstC2CpgFrontend extends LanguageFrontend {
  override type ConfigType = Config

  def execute(sourceCodePath: java.io.File): Cpg = {
    val cpg          = newEmptyCpg()
    val pathAsString = sourceCodePath.getAbsolutePath
    val config = getConfig()
      .getOrElse(Config())
      .withInputPath(pathAsString)
      .withSchemaValidation(ValidationMode.Enabled)

    val preprocessedFiles = if (config.withPreprocessedFiles) {
      SourceFiles
        .determine(
          config.inputPath,
          Set(FileDefaults.PreprocessedExt),
          ignoredDefaultRegex = Option(DefaultIgnoredFolders),
          ignoredFilesRegex = Option(config.ignoredFilesRegex),
          ignoredFilesPath = Option(config.ignoredFiles)
        )
    } else List.empty

    val sourceFileExtensions = FileDefaults.SourceFileExtensions ++
      Option.when(config.withPreprocessedFiles)(FileDefaults.PreprocessedExt).toList
    val sourcePass = new AstCreationPass(cpg, preprocessedFiles, sourceFileExtensions, config)
    sourcePass.createAndApply()
    val headerPass =
      new AstCreationPass(
        cpg,
        preprocessedFiles,
        FileDefaults.HeaderFileExtensions,
        config,
        previousAccumulator = Some(sourcePass.accumulatedState())
      )
    headerPass.createAndApply()
    new FunctionDeclNodePass(cpg, headerPass.unhandledMethodDeclarations(), config).createAndApply()
    new PostFrontendValidator(cpg, ValidationLevel.V1).run()
    cpg
  }
}
