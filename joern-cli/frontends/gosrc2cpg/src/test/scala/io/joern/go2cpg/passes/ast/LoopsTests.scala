package io.joern.go2cpg.passes.ast

import io.joern.go2cpg.testfixtures.GoCodeToCpgSuite
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.codepropertygraph.generated.{ControlStructureTypes, Operators}
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.language.operatorextension.OpNodes

import scala.collection.immutable.List

class LoopsTests extends GoCodeToCpgSuite {
  "AST Structure for for-loops" should {
    "be correct for for-loop case 1" in {
      val cpg = code("""
         |package main
         |
         |func main() {
         |   var b int
         |
         |   /* for loop execution */
         |   for a := 0; a < 10; a++ {
         |       b += a
         |   }
         |}
         |""".stripMargin)

      inside(cpg.method.name("main").controlStructure.l) { case List(forStmt) =>
        forStmt.controlStructureType shouldBe ControlStructureTypes.FOR
        inside(forStmt.astChildren.order(1).l) { case List(initializerBlock: Block) =>
          initializerBlock.astChildren.isCall.code.l shouldBe List("a := 0")
        }
        inside(forStmt.astChildren.order(2).l) { case List(lessThanCall: Call) =>
          lessThanCall.code shouldBe "a < 10"
        }
        inside(forStmt.astChildren.order(3).l) { case List(increCall: Call) =>
          increCall.code shouldBe "a++"
        }
        inside(forStmt.astChildren.order(4).l) { case List(body: Block) =>
          body.astChildren.isCall.code.l shouldBe List("b += a")
        }
      }
    }

    "be correct for for-loop case 2" in {
      val cpg = code("""
         |package main
         |
         |func main() {
         |   var b int = 15
         |   var a int
         |
         |   /* for loop execution */
         |   for a < b {
         |       a++
         |   }
         |}
         |""".stripMargin)

      inside(cpg.method.name("main").controlStructure.l) { case List(forStmt) =>
        forStmt.controlStructureType shouldBe ControlStructureTypes.FOR
        inside(forStmt.astChildren.order(1).l) { case List(initializerBlock: Block) =>
          initializerBlock.astChildren.size shouldBe 0
        }
        inside(forStmt.astChildren.order(2).l) { case List(lessThanCall: Call) =>
          lessThanCall.code shouldBe "a < b"
        }

        forStmt.astChildren.order(3).size shouldBe 0

        inside(forStmt.astChildren.order(4).l) { case List(body: Block) =>
          body.astChildren.isCall.code.l shouldBe List("a++")
        }
      }
    }

    "for-loop without breaking condition" in {
      val cpg = code("""
          package main
          |var a = 10
          |func main() {
          |	for {
          | a++
          |	}
          |}
          |
          |""".stripMargin)

      inside(cpg.method.name("main").controlStructure.l) { case List(forStmt) =>
        forStmt.controlStructureType shouldBe ControlStructureTypes.FOR
        inside(forStmt.astChildren.order(1).l) { case List(initializerBlock: Block) =>
          initializerBlock.astChildren.size shouldBe 0
        }

        // order 2 is braking condition
        forStmt.astChildren.order(2).size shouldBe 0
        forStmt.astChildren.order(3).size shouldBe 0

        inside(forStmt.astChildren.order(4).l) { case List(body: Block) =>
          body.astChildren.isCall.code.l shouldBe List("a++")
        }
      }
    }
    "be correct for for-loop nested structure" in {
      val cpg = code("""
         |package main
         |import "fmt"
         |func main() {
         |   var i, j int
         |   counter := 0
         |   for i = 2; i < 100; i++ {
         |      for j = 2; j <= (i/j); j++ {
         |         if(i%j==0) {
         |            counter++
         |         }
         |      }
         |   }
         |}
         |""".stripMargin)

      inside(cpg.method.name("main").controlStructure.l) { case List(outerForStmt, innerForStmt, ifStmt) =>
        outerForStmt.controlStructureType shouldBe ControlStructureTypes.FOR
        inside(outerForStmt.astChildren.order(1).l) { case List(initializerBlock: Block) =>
          initializerBlock.astChildren.isCall.code.l shouldBe List("i = 2")
        }
        inside(outerForStmt.astChildren.order(2).l) { case List(lessThanCall: Call) =>
          lessThanCall.code shouldBe "i < 100"
        }
        inside(outerForStmt.astChildren.order(3).l) { case List(increCall: Call) =>
          increCall.code shouldBe "i++"
        }

        outerForStmt.astChildren.ast.isControlStructure.l shouldBe List(innerForStmt, ifStmt)

        inside(innerForStmt.astChildren.order(1).l) { case List(initializerBlock: Block) =>
          initializerBlock.astChildren.isCall.code.l shouldBe List("j = 2")
        }
        inside(innerForStmt.astChildren.order(2).l) { case List(lessThanCall: Call) =>
          lessThanCall.code shouldBe "j <= (i/j)"
        }
        inside(innerForStmt.astChildren.order(3).l) { case List(increCall: Call) =>
          increCall.code shouldBe "j++"
        }

        innerForStmt.astChildren.ast.isControlStructure.l shouldBe List(ifStmt)
      }
    }

    "ast creation for break, continue, goto" should {

      "be correct" in {

        val cpg = code("""
           |package main
           |
           |func main() {
           |   var b int
           |   for a := 0; a < 10; a++ {
           |       b += a
           |       if (b == 8) {
           |         break;
           |       }
           |       if (b < 3) {
           |         continue;
           |       }
           |       if (b == 5) {
           |          goto End
           |       }
           |   }
           |  End:
           |     b = 9
           |}
           |""".stripMargin)

        inside(cpg.method.name("main").controlStructure.l) {
          case List(forStmt, ifStmtFirst, breakStmt, ifStmtSecond, continueStmt, ifStmtThird, gotoStmt) =>
            breakStmt.controlStructureType shouldBe ControlStructureTypes.BREAK
            breakStmt.code shouldBe "break"

            continueStmt.controlStructureType shouldBe ControlStructureTypes.CONTINUE
            continueStmt.code shouldBe "continue"

            gotoStmt.controlStructureType shouldBe ControlStructureTypes.GOTO
            gotoStmt.code shouldBe "goto End"
        }
      }
    }
  }

  "be correct for for-loop case 3" should {
    val cpg = code("""
        |package main
        |import "fmt"
        |func main() {
        |   message := "Hello, Gophers!"
        |
        |   var counter int
        |   for index, char := range message {
        |        counter++
        |    }
        |}
        |""".stripMargin)

    "check working AST structure is in place" in {
      val List(forStmt) = cpg.method.name("main").controlStructure.l
      forStmt.controlStructureType shouldBe ControlStructureTypes.FOR
      val List(identifier: Identifier) = forStmt.astChildren.order(1).l: @unchecked
      identifier.name shouldBe "message"
    }

    "TODO needs correction and non working" ignore {
      val List(forStmt)           = cpg.method.name("main").controlStructure.l
      val List(localBlock: Block) = forStmt.astChildren.order(2).l: @unchecked
      localBlock.astChildren.isLocal.code.l shouldBe List("index", "char")

      val List(assignCall: Call) = forStmt.astChildren.order(3).l: @unchecked
      assignCall.code shouldBe "index, char := range message"

      val List(body: Block) = forStmt.astChildren.order(4).l: @unchecked
      body.astChildren.isCall.code.l shouldBe List("counter++")
    }
  }

  "for loop with range not, having any index" should {
    val cpg = code("""
        |package main
        |
        |func main() {
        |
        |	for range servers {
        |       b := 10
        |		success = success && <-shutdownChan
        |	}
        |}
        |
        |""".stripMargin)

    "check nodes in body" in {
      val List(identifierNode) = cpg.identifier("b").l
      identifierNode.typeFullName shouldBe "int"
    }

    "check working AST structure is in place" in {
      val List(forStmt) = cpg.method.name("main").controlStructure.l
      forStmt.controlStructureType shouldBe ControlStructureTypes.FOR
      val List(identifier: Identifier) = forStmt.astChildren.order(1).l: @unchecked
      identifier.name shouldBe "servers"
    }
  }
}
