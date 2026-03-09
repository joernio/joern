package io.joern.pysrc2cpg.testfixtures

import io.joern.dataflowengineoss.DefaultSemantics
import io.joern.dataflowengineoss.language.Path
import io.joern.dataflowengineoss.semanticsloader.Semantics
import io.joern.dataflowengineoss.testfixtures.SemanticCpgTestFixture
import io.joern.dataflowengineoss.testfixtures.SemanticTestCpg
import io.joern.pysrc2cpg.Py2CpgOnFileSystem
import io.joern.pysrc2cpg.Py2CpgOnFileSystemConfig
import io.joern.x2cpg.ValidationMode
import io.joern.x2cpg.frontendspecific.pysrc2cpg.DynamicTypeHintFullNamePass
import io.joern.x2cpg.frontendspecific.pysrc2cpg.ImportsPass
import io.joern.x2cpg.frontendspecific.pysrc2cpg.PythonImportResolverPass
import io.joern.x2cpg.frontendspecific.pysrc2cpg.PythonInheritanceNamePass
import io.joern.x2cpg.frontendspecific.pysrc2cpg.PythonTypeHintCallLinker
import io.joern.x2cpg.frontendspecific.pysrc2cpg.PythonTypeRecoveryPassGenerator
import io.joern.x2cpg.passes.base.AstLinkerPass
import io.joern.x2cpg.passes.callgraph.NaiveCallLinker
import io.joern.x2cpg.testfixtures.Code2CpgFixture
import io.joern.x2cpg.testfixtures.DefaultTestCpg
import io.joern.x2cpg.testfixtures.LanguageFrontend
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.semanticcpg.language.ICallResolver
import io.shiftleft.semanticcpg.language.NoResolve
import io.shiftleft.semanticcpg.validation.PostFrontendValidator

trait PythonFrontend extends LanguageFrontend {
  override type ConfigType = Py2CpgOnFileSystemConfig

  override val fileSuffix: String = ".py"

  def schemaValidation: ValidationMode = ValidationMode.Enabled
  def withFileContent: Boolean         = true

  override def execute(sourceCodePath: java.io.File): Cpg = {
    val tmp = new Py2CpgOnFileSystem()
      .createCpg(
        Py2CpgOnFileSystemConfig()
          .withSchemaValidation(schemaValidation)
          .withDisableFileContent(!withFileContent)
          .withInputPath(sourceCodePath.getAbsolutePath)
      )
      .get
    new PostFrontendValidator(tmp, false).run()
    tmp
  }
}

class PySrcTestCpg(schemaValidationMode: ValidationMode, fileContentEnabled: Boolean)
    extends DefaultTestCpg
    with PythonFrontend
    with SemanticTestCpg {

  override def schemaValidation: ValidationMode = schemaValidationMode
  override def withFileContent: Boolean         = fileContentEnabled

  override protected def applyPasses(): Unit = {
    super.applyPasses()
    if (!_withPostProcessing) applyOssDataFlow()
  }

  override def applyPostProcessingPasses(): Unit = {
    new ImportsPass(this).createAndApply()
    new PythonImportResolverPass(this).createAndApply()
    new PythonInheritanceNamePass(this).createAndApply()
    new DynamicTypeHintFullNamePass(this).createAndApply()
    new PythonTypeRecoveryPassGenerator(this).generate().foreach(_.createAndApply())
    new PythonTypeHintCallLinker(this).createAndApply()
    new NaiveCallLinker(this).createAndApply()

    // Some of the passes above create new methods, so, we
    // need to run the ASTLinkerPass one more time
    new AstLinkerPass(this).createAndApply()
    applyOssDataFlow()
  }

}

class PySrc2CpgFixture(
  withOssDataflow: Boolean = false,
  semantics: Semantics = DefaultSemantics(),
  withPostProcessing: Boolean = true,
  withSchemaValidation: ValidationMode = ValidationMode.Enabled,
  withFileContent: Boolean = true
) extends Code2CpgFixture(() =>
      new PySrcTestCpg(withSchemaValidation, withFileContent)
        .withOssDataflow(withOssDataflow)
        .withSemantics(semantics)
        .withPostProcessingPasses(withPostProcessing)
    )
    with SemanticCpgTestFixture(semantics) {

  implicit val resolver: ICallResolver = NoResolve

  protected def flowToResultPairs(path: Path): List[(String, Integer)] =
    path.resultPairs().collect { case (firstElement: String, secondElement) =>
      (firstElement, secondElement.getOrElse(-1))
    }
}
