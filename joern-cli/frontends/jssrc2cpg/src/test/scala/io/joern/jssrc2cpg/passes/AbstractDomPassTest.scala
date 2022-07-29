package io.joern.jssrc2cpg.passes

import io.shiftleft.codepropertygraph.generated.nodes.Expression
import io.shiftleft.codepropertygraph.generated.nodes.TemplateDom
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.NodeTypes
import io.shiftleft.semanticcpg.language._
import org.apache.commons.lang.StringUtils
import overflowdb.traversal.InitialTraversal
import overflowdb.traversal.Traversal

abstract class AbstractDomPassTest extends AbstractPassTest {

  protected def templateDom(cpg: Cpg): Traversal[TemplateDom] =
    InitialTraversal.from[TemplateDom](cpg.graph, NodeTypes.TEMPLATE_DOM)

  protected def templateDomName(cpg: Cpg): Set[String] =
    templateDom(cpg).name.toSetImmutable

  protected def templateDomCode(cpg: Cpg): List[String] =
    templateDom(cpg).code.map(StringUtils.normalizeSpace).l

  protected def parentTemplateDom(c: Expression): TemplateDom =
    c.parentExpression.get.asInstanceOf[TemplateDom]

}
