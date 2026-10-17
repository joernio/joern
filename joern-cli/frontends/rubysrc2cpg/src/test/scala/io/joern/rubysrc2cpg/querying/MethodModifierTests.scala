package io.joern.rubysrc2cpg.querying

import io.joern.rubysrc2cpg.testfixtures.RubyCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.ModifierTypes
import io.shiftleft.semanticcpg.language.*

class MethodModifierTests extends RubyCode2CpgFixture {

  "class methods" should {
    val cpg = code("""
        |class MyClass
        |  def my_instance_method
        |  end
        |
        |  def self.my_static_method
        |  end
        |end
        |""".stripMargin)

    "attach STATIC to singleton methods" in {
      cpg.method.name("my_static_method").modifier.modifierType.l should contain(ModifierTypes.STATIC)
    }

    "not attach STATIC to instance methods" in {
      cpg.method.name("my_instance_method").modifier.modifierType.l should not contain ModifierTypes.STATIC
    }
  }
}
