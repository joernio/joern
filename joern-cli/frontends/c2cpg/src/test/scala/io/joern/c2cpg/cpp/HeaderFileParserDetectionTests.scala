package io.joern.c2cpg.cpp

import io.joern.c2cpg.testfixtures.C2CpgSuite
import io.shiftleft.semanticcpg.language.*

class HeaderFileParserDetectionTests extends C2CpgSuite {

  "detecting the correct parser for header files" should {

    "work for transitive includes" in {
      val cpg = code(
        """
          |#include "util.h"
          |class Foo {}
          |""".stripMargin,
        "main.h"
      ).moreCode(
        """
          |int utilFunc(int a) { return 0; }
          |""".stripMargin,
        "util.h"
      ).moreCode(
        """
          |#include "main.h"
          |int Foo::fooA(int a) { return 0; }
          |""".stripMargin,
        "main.cpp"
      ).moreCode(
        """
          |#include "util.h"
          |int fooA(int a) { return 0; }
          |""".stripMargin,
        "main.c"
      )
      cpg.typeDecl.nameNot("<global>").internal.map(t => (t.fullName, t.file.name.head)).sorted.l shouldBe List(
        // even though class Foo is defined in a .h file we parse it as C++ because it's included in a .cpp file:
        ("Foo", "main.h"),
        ("Foo.fooA:int(int)", "main.cpp"),
        ("fooA", "main.c"),
        // parsed the first time as C code because it's included in a .c file (main.c -> util.h):
        ("utilFunc", "util.h"),
        // parsed the second time as C++ code because it's included in a .cpp file (main.cpp -> main.h -> util.h):
        ("utilFunc:int(int)", "util.h")
      )
    }

    "work for includes in other header files" in {
      val cpg = code(
        """
          |int utilFunc(int a) { return 0; }
          |""".stripMargin,
        "util.h"
      ).moreCode(
        """
          |#include "util.h"
          |int fooA(int a) { return 0; }
          |""".stripMargin,
        "main.c"
      ).moreCode(
        """
          |#include "util.h"
          |""".stripMargin,
        "header.hpp"
      )
      cpg.typeDecl.nameNot("<global>").internal.map(t => (t.fullName, t.file.name.head)).sorted.l shouldBe List(
        ("fooA", "main.c"),
        // parsed the first time as C code because it's included in a .c file (main.c -> util.h):
        ("utilFunc", "util.h"),
        // parsed the second time as C++ code because it's included in a .hpp file (header.hpp -> util.h):
        ("utilFunc:int(int)", "util.h")
      )
    }

  }

}
