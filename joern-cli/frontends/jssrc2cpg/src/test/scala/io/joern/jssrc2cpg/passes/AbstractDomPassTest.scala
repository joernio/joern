package io.joern.jssrc2cpg.passes

import io.shiftleft.codepropertygraph.generated.nodes.Expression
import io.shiftleft.codepropertygraph.generated.nodes.TemplateDom
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.semanticcpg.language._
import org.apache.commons.lang.StringUtils

abstract class AbstractDomPassTest extends AbstractPassTest {

  protected def templateDomName(cpg: Cpg): Set[String] =
    cpg.templateDom.name.toSetImmutable

  protected def templateDomCode(cpg: Cpg): List[String] =
    cpg.templateDom.code.map(StringUtils.normalizeSpace).l

  protected def parentTemplateDom(c: Expression): TemplateDom =
    c.parentExpression.isTemplateDom.head

}
