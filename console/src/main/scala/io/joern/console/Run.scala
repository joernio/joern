package io.joern.console

import io.shiftleft.passes.CpgPass
import io.shiftleft.semanticcpg.language.HasStoreMethod
import io.shiftleft.semanticcpg.layers.{LayerCreator, LayerCreatorContext}
import org.reflections8.Reflections
import org.reflections8.util.{ClasspathHelper, ConfigurationBuilder}

import scala.jdk.CollectionConverters.*

object Run {

  def runCustomQuery(console: Console[?], query: HasStoreMethod): Unit = {
    console._runAnalyzer(new LayerCreator {
      override val overlayName: String = "custom"
      override val description: String = "A custom pass"

      override def create(context: LayerCreatorContext): Unit = {
        val pass: CpgPass = new CpgPass(console.cpg) {
          override val name = "custom"
          override def run(builder: DiffGraphBuilder): Unit = {
            query.store()(builder)
          }
        }
        pass.createAndApply()
      }
    })
  }

  /** Generate code for the run command
    * @param exclude
    *   list of analyzers to exclude (by full class name)
    */
  def codeForRunCommand(exclude: List[String] = List()): String = {
    val ioSlLayerCreators    = creatorsFor("io.shiftleft", exclude)
    val ioJoernLayerCreators = creatorsFor("io.joern", exclude)
    codeForLayerCreators((ioSlLayerCreators ++ ioJoernLayerCreators).distinct)
  }

  private def creatorsFor(namespace: String, exclude: List[String]) = {
    new Reflections(
      new ConfigurationBuilder().setUrls(
        ClasspathHelper.forPackage(namespace, ClasspathHelper.contextClassLoader(), ClasspathHelper.staticClassLoader())
      )
    ).getSubTypesOf(classOf[LayerCreator])
      .asScala
      .filterNot(t => t.isAnonymousClass || t.isLocalClass || t.isMemberClass || t.isSynthetic)
      .filterNot(t => t.getName.startsWith("io.joern.console.Run"))
      .toList
      .map(t => (t.getSimpleName.toLowerCase, s"_root_.${t.getName}"))
      .filter(t => !exclude.contains(t._2))
  }

  private def codeForLayerCreators(layerCreatorTypeNames: List[(String, String)]): String = {
    val optsMembersCode = layerCreatorTypeNames
      .map { case (varName, typeName) => s"val $varName = $typeName.defaultOpts" }
      .mkString("\n")

    val optsCode =
      s"""
         |class OptsDynamic {
         |$optsMembersCode
         |}
         |
         |val opts = new OptsDynamic()
         |
         | import _root_.io.shiftleft.codepropertygraph.generated.DiffGraphBuilder
         | implicit def _diffGraph: DiffGraphBuilder = opts.commit.diffGraphBuilder
         | def diffGraph = _diffGraph
         |""".stripMargin

    val membersCode = layerCreatorTypeNames
      .map { case (varName, typeName) => s"  def $varName: Cpg = _runAnalyzer(new $typeName(opts.$varName))" }
      .mkString("\n")

    val toStringCode =
      s"""
         |  import flatgraph.help.Table
         |  override def toString() : String = {
         |    val columnNames = List("name", "description")
         |    val rows =
         |      ${layerCreatorTypeNames.map { case (varName, typeName) =>
          s"""List("$varName",$typeName.description.trim)"""
        }}
         |    "\\n" + Table(columnNames, rows).render
         |  }
         |""".stripMargin

    optsCode +
      s"""
         |class OverlaysDynamic {
         |
         |  def apply(query: _root_.io.shiftleft.semanticcpg.language.HasStoreMethod) =
         |    _root_.io.joern.console.Run.runCustomQuery(console, query)
         |
         |$membersCode
         |
         |$toStringCode
         |}
         |val run = new OverlaysDynamic()
         |""".stripMargin
  }

}
