package io.joern.rust2cpg.passes.ast

import io.joern.rust2cpg.testfixtures.Rust2CpgSuite
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*

class UseTests extends Rust2CpgSuite(noSysRoot = true) {

  "use" should {
    val cpg = code("use std::collections::HashMap;")

    "have correct properties" in {
      inside(cpg.imports.l) { case use :: Nil =>
        use.code shouldBe "use std::collections::HashMap;"
        use.importedEntity shouldBe Some("std::collections::HashMap")
        use.importedAs shouldBe Some("HashMap")
        use.isWildcard shouldBe None
      }
    }
  }

  "rename" should {
    val cpg = code("use std::collections::HashMap as Map;")

    "have correct properties" in {
      inside(cpg.imports.l) { case use :: Nil =>
        use.code shouldBe "use std::collections::HashMap as Map;"
        use.importedEntity shouldBe Some("std::collections::HashMap")
        use.importedAs shouldBe Some("Map")
      }
    }
  }

  "underscore rename" should {
    val cpg = code("use foo::Bar as _;")

    "have correct properties" in {
      inside(cpg.imports.l) { case use :: Nil =>
        use.importedEntity shouldBe Some("foo::Bar")
        use.importedAs shouldBe Some("_")
      }
    }
  }

  "wildcard" should {
    val cpg = code("use std::io::*;")

    "have correct properties" in {
      inside(cpg.imports.l) { case use :: Nil =>
        use.code shouldBe "use std::io::*;"
        use.importedEntity shouldBe Some("std::io")
        use.importedAs shouldBe Some("*")
        use.isWildcard shouldBe Some(true)
      }
    }
  }

  "nested wildcard" should {
    val cpg = code("use a::{b::*};")

    "have correct properties" in {
      inside(cpg.imports.l) { case use :: Nil =>
        use.importedEntity shouldBe Some("a::b")
        use.importedAs shouldBe Some("*")
        use.isWildcard shouldBe Some(true)
      }
    }
  }

  "relative paths" should {
    val cpg = code("""
        |use crate::a::B;
        |use super::x::Y;
        |use self::m::N;
        |""".stripMargin)

    "have correct properties" in {
      inside(cpg.imports.sortBy(_.lineNumber).l) { case useB :: useY :: useN :: Nil =>
        useB.importedEntity shouldBe Some("crate::a::B")
        useB.importedAs shouldBe Some("B")
        useY.importedEntity shouldBe Some("super::x::Y")
        useY.importedAs shouldBe Some("Y")
        useN.importedEntity shouldBe Some("self::m::N")
        useN.importedAs shouldBe Some("N")
      }
    }
  }

  "use without prefix" should {
    val cpg = code("use {a, b::C};")

    "have correct properties" in {
      inside(cpg.imports.sortBy(_.code).l) { case useA :: useC :: Nil =>
        useA.importedEntity shouldBe Some("a")
        useA.importedAs shouldBe Some("a")
        useC.importedEntity shouldBe Some("b::C")
        useC.importedAs shouldBe Some("C")
      }
    }
  }

  "nested use tree" should {
    val cpg = code("use a::{b::C, d::{E as F, self}};")

    "have correct properties" in {
      inside(cpg.imports.sortBy(_.code).l) { case useC :: useE :: useD :: Nil =>
        useC.importedEntity shouldBe Some("a::b::C")
        useC.importedAs shouldBe Some("C")
        useE.importedEntity shouldBe Some("a::d::E")
        useE.importedAs shouldBe Some("F")
        useD.importedEntity shouldBe Some("a::d")
        useD.importedAs shouldBe Some("d")
      }
    }

  }

  "trailing self" should {
    val cpg = code("use std::io::self;")

    "have correct properties" in {
      inside(cpg.imports.l) { case use :: Nil =>
        use.importedEntity shouldBe Some("std::io")
        use.importedAs shouldBe Some("io")
      }
    }
  }

  "renamed self" should {
    val cpg = code("use m::{self as alias};")

    "have correct properties" in {
      inside(cpg.imports.l) { case use :: Nil =>
        use.importedEntity shouldBe Some("m")
        use.importedAs shouldBe Some("alias")
      }
    }
  }

}
