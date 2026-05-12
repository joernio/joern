package io.joern.rust2cpg.passes.ast

import io.joern.rust2cpg.testfixtures.Rust2CpgSuite
import io.shiftleft.codepropertygraph.generated.{EvaluationStrategies, NodeTypes}
import io.shiftleft.semanticcpg.language.*

class ImplTests extends Rust2CpgSuite(noSysRoot = true) {

  "`impl X` block methods have correct fullnames" in {
    val cpg = code("""
        |struct Point { x: i32 }
        |impl Point {
        |    fn new() -> Point { Point { x: 0 } }
        |    fn get_x(&self) -> i32 { self.x }
        |}
        |""".stripMargin)

    inside(cpg.method.name("new").l) { case newFn :: Nil =>
      newFn.fullName shouldBe "rust2cpgtest::Point::new"
      newFn.astParentFullName shouldBe "rust2cpgtest::Point"
      newFn.parameter shouldBe empty
    }

    inside(cpg.method.name("get_x").l) { case getX :: Nil =>
      getX.fullName shouldBe "rust2cpgtest::Point::get_x"
      getX.astParentFullName shouldBe "rust2cpgtest::Point"
      inside(getX.parameter.l) { case self :: Nil =>
        self.name shouldBe "self"
        self.index shouldBe 0
        self.typeFullName shouldBe "rust2cpgtest::Point"
        self.evaluationStrategy shouldBe EvaluationStrategies.BY_REFERENCE
      }
    }
  }

  "`self` parameter has correct evaluation strategy" in {
    val cpg = code("""
        |struct Foo {}
        |impl Foo {
        |    fn by_value(self) {}
        |    fn by_ref(&self) {}
        |    fn by_mut_ref(&mut self) {}
        |}
        |""".stripMargin)

    cpg.method.name("by_value").parameter.evaluationStrategy.l shouldBe List(EvaluationStrategies.BY_SHARING)
    cpg.method.name("by_ref").parameter.evaluationStrategy.l shouldBe List(EvaluationStrategies.BY_REFERENCE)
    cpg.method.name("by_mut_ref").parameter.evaluationStrategy.l shouldBe List(EvaluationStrategies.BY_REFERENCE)
  }

  "impl methods have correct parameter indices" in {
    val cpg = code("""
        |struct Foo {}
        |impl Foo {
        |    fn add(&self, x: i32, y: i32) -> i32 { x + y }
        |    fn do_stuff(x: i32, y: i32) -> i32 { x * y }
        |}
        |""".stripMargin)

    inside(cpg.method.name("add").parameter.sortBy(_.index).l) { case self :: x :: y :: Nil =>
      self.name shouldBe "self"
      self.typeFullName shouldBe "rust2cpgtest::Foo"
      self.index shouldBe 0
      x.name shouldBe "x"
      x.typeFullName shouldBe "i32"
      x.index shouldBe 1
      y.name shouldBe "y"
      y.typeFullName shouldBe "i32"
      y.index shouldBe 2
    }

    inside(cpg.method.name("do_stuff").parameter.sortBy(_.index).l) { case x :: y :: Nil =>
      x.name shouldBe "x"
      x.typeFullName shouldBe "i32"
      x.index shouldBe 1
      y.name shouldBe "y"
      y.typeFullName shouldBe "i32"
      y.index shouldBe 2
    }
  }
}
