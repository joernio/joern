import io.shiftleft.codepropertygraph.schema.*
import overflowdb.codegen.CodeGen
import overflowdb.schema.SchemaBuilder
import overflowdb.schema.Property.ValueType

import java.io.File

object CpgExtCodegen {
  def main(args: Array[String]): Unit = {
    val outputDir = args.headOption
      .map(new File(_))
      .getOrElse(throw new AssertionError("please pass outputDir as first parameter"))

    val builder   = new SchemaBuilder(domainShortName = "Cpg", basePackage = "io.shiftleft.codepropertygraph.generated")
    val cpgSchema = new CpgSchema(builder)

    // START extensions for this build - add your's here and remove the example properties
    val exampleProperty =
      builder.addProperty(name = "EXAMPLE_PROPERTY", valueType = ValueType.String, comment = "an example property")

    val exampleNode =
      builder.addNodeType(name = "EXAMPLE_NODE", comment = "an example node").addProperties(exampleProperty)

    cpgSchema.fs.file.addProperties(exampleProperty)
    // END extensions for this build

    new CodeGen(builder.build).run(outputDir)
  }
}
