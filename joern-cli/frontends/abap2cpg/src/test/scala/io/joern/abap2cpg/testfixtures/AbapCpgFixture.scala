package io.joern.abap2cpg.testfixtures

import io.joern.abap2cpg.parser.AbapIntermediateAst.*
import io.joern.abap2cpg.passes.{AstCreator, RefEdgePass}
import io.joern.x2cpg.ValidationMode
import io.joern.x2cpg.passes.base.ContainsEdgePass
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.semanticcpg.language.*
import flatgraph.DiffGraphApplier
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

/** Helper for building an in-memory CPG from a ProgramRoot without requiring the abapgen binary.
  */
trait AbapCpgFixture extends AnyWordSpec with Matchers {

  val noSpan: TextSpan = TextSpan()

  def cpgForProgram(program: ProgramRoot): Cpg = {
    implicit val schemaValidation: ValidationMode = ValidationMode.Disabled
    val cpg                                       = Cpg.empty
    val astCreator                                = new AstCreator(program, program.fileName)
    val generated                                 = astCreator.createAst()
    DiffGraphApplier.applyDiff(cpg.graph, generated)
    new ContainsEdgePass(cpg).createAndApply()
    new RefEdgePass(cpg).createAndApply()
    cpg
  }

  /** Build a simple ProgramRoot with a single standalone method. */
  def programWithMethod(
    methodName: String,
    importing: Seq[Parameter] = Seq.empty,
    exporting: Seq[Parameter] = Seq.empty,
    changing: Seq[Parameter] = Seq.empty,
    returning: Option[Parameter] = None,
    body: Option[StatementList] = None
  ): ProgramRoot = {
    val method = MethodDef(
      name = methodName,
      visibility = None,
      isStatic = false,
      parameters = MethodParameters(
        importing = importing,
        exporting = exporting,
        changing = changing,
        returning = returning
      ),
      body = body,
      span = noSpan
    )
    ProgramRoot(
      fileName = "test.abap",
      objectType = "PROG",
      methods = Seq(method),
      span = noSpan
    )
  }

  /** Build a ProgramRoot with a class containing one method. */
  def programWithClass(
    className: String,
    methodName: String,
    importing: Seq[Parameter] = Seq.empty,
    body: Option[StatementList] = None
  ): ProgramRoot = {
    val method = MethodDef(
      name = methodName,
      visibility = Some("PUBLIC"),
      isStatic = false,
      parameters = MethodParameters(importing = importing),
      body = body,
      span = noSpan
    )
    val classDef = ClassDef(
      name = className,
      visibility = "PUBLIC",
      methods = Seq(method),
      span = noSpan
    )
    ProgramRoot(
      fileName = "test.abap",
      objectType = "CLAS",
      classes = Seq(classDef),
      span = noSpan
    )
  }
}
