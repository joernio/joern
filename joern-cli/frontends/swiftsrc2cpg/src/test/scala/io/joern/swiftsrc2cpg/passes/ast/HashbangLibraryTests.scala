// This test file has been translated from swift/test/Parse/hashbang_library.swift

package io.joern.swiftsrc2cpg.passes.ast

import io.shiftleft.codepropertygraph.generated._
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.semanticcpg.language._

class HashbangLibraryTests extends AbstractPassTest {

  "HashbangLibraryTests" should {

    "testHashbangLibrary1" ignore AstFixture("""
        |#!/usr/bin/swift
        |class Foo {}
        |""".stripMargin) { cpg => ??? }

  }

}
