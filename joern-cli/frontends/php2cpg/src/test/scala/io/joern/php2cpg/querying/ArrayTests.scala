package io.joern.php2cpg.querying

import io.joern.php2cpg.testfixtures.PhpCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.codepropertygraph.generated.nodes.{Block, Call, Identifier, Literal, Local}
import io.shiftleft.semanticcpg.language.*

class ArrayTests extends PhpCode2CpgFixture {
  "array accesses with variable keys should be represented as index accesses" in {
    val cpg = code("""<?php
        |$array[$key];
        |""".stripMargin)

    inside(cpg.call.l) { case List(indexAccess) =>
      indexAccess.name shouldBe Operators.indexAccess
      indexAccess.code shouldBe "$array[$key]"
      indexAccess.lineNumber shouldBe Some(2)

      inside(indexAccess.argument.l) { case List(array: Identifier, key: Identifier) =>
        array.name shouldBe "array"
        array.code shouldBe "$array"
        array.lineNumber shouldBe Some(2)

        key.name shouldBe "key"
        key.code shouldBe "$key"
        key.lineNumber shouldBe Some(2)
      }
    }
  }

  "array accesses with literal keys should be represented as index accesses" in {
    val cpg = code("""<?php
        |$array[0];
        |""".stripMargin)

    inside(cpg.call.l) { case List(indexAccess) =>
      indexAccess.name shouldBe Operators.indexAccess
      indexAccess.code shouldBe "$array[0]"
      indexAccess.lineNumber shouldBe Some(2)

      inside(indexAccess.argument.l) { case List(array: Identifier, key: Literal) =>
        array.name shouldBe "array"
        array.code shouldBe "$array"
        array.lineNumber shouldBe Some(2)

        key.code shouldBe "0"
        key.lineNumber shouldBe Some(2)
      }
    }
  }

  "array accesses with multiple literal keys should be represented as index accesses" in {
    val cpg = code("""<?php
        |$arr[1][2];
        |""".stripMargin)

    inside(cpg.method.name("<global>").block.astChildren.l) { case List(local: Local, indexAccess: Call) =>
      local.name shouldBe "arr"
      local.code shouldBe "$arr"

      indexAccess.name shouldBe Operators.indexAccess
      indexAccess.code shouldBe "$arr[1][2]"

      inside(indexAccess.argument.l) { case List(indexAccessTwo: Call, twoLiteral: Literal) =>
        twoLiteral.code shouldBe "2"

        indexAccessTwo.name shouldBe Operators.indexAccess
        indexAccessTwo.code shouldBe "$arr[1]"

        inside(indexAccessTwo.argument.l) { case List(identifier: Identifier, twoLiteral: Literal) =>
          twoLiteral.code shouldBe "1"

          identifier.name shouldBe "arr"
          identifier.code shouldBe "$arr"
        }
      }
    }
  }

  "assignments using the empty array dimension fetch syntax should be rewritten as array_push" in {
    val cpg = code("""<?php
        |function foo($val) {
        |  $xs[] = $val;
        |}
        |""".stripMargin)

    inside(cpg.method.name("foo").body.astChildren.l) { case List(xsLocal: Local, arrayPush: Call) =>
      xsLocal.name shouldBe "xs"
      xsLocal.lineNumber shouldBe Some(3)

      arrayPush.name shouldBe "array_push"
      arrayPush.code shouldBe "$xs[] = $val"
    }
  }

  "assignments using the multi array dimension fetch syntax without last dimension should be rewritten as multiple assignments" in {
    val cpg = code("""<?php
        |function foo($val) {
        |  $xs[][2][] = $val;
        |}
        |""".stripMargin)

    inside(cpg.method.name("foo").body.astChildren.l) { case List(block: Block) =>
      block.code shouldBe "$xs[][2][] = $val"
      block.lineNumber shouldBe Some(3)

      inside(block.astChildren.isLocal.l) { case List(tmp0, tmp1, tmp2, tmp3, xs) =>
        tmp0.name shouldBe "foo@tmp-0"
        tmp0.code shouldBe "$foo@tmp-0"
        tmp1.name shouldBe "foo@tmp-1"
        tmp1.code shouldBe "$foo@tmp-1"
        tmp2.name shouldBe "foo@tmp-2"
        tmp2.code shouldBe "$foo@tmp-2"
        tmp3.name shouldBe "foo@tmp-3"
        tmp3.code shouldBe "$foo@tmp-3"
        xs.name shouldBe "xs"
        xs.code shouldBe "$xs"
      }

      inside(block.astChildren.not(_.isLocal).l) {
        case List(assignOne: Call, assignTwo: Call, arrayPushCall: Call, retIden: Identifier) =>
          assignOne.name shouldBe Operators.assignment
          assignOne.code shouldBe "$foo@tmp-0 = array($val)"
          assignOne.order shouldBe 1
          inside(assignOne.astChildren.l) { case List(lhsIdentifier: Identifier, rhsBlock: Block) =>
            lhsIdentifier.name shouldBe "foo@tmp-0"
            lhsIdentifier.code shouldBe "$foo@tmp-0"

            inside(rhsBlock.astChildren.l) {
              case List(arrayAssignCall: Call, indexAssignCall: Call, retIdentifier: Identifier) =>
                arrayAssignCall.name shouldBe Operators.assignment
                arrayAssignCall.code shouldBe "$foo@tmp-1 = array()"

                indexAssignCall.name shouldBe Operators.assignment
                indexAssignCall.code shouldBe "$foo@tmp-1[0] = $val"

                retIdentifier.name shouldBe "foo@tmp-1"
                retIdentifier.code shouldBe "$foo@tmp-1"
            }
          }

          assignTwo.name shouldBe Operators.assignment
          assignTwo.code shouldBe "$foo@tmp-2 = array(2 => $foo@tmp-0)"
          assignTwo.order shouldBe 2
          inside(assignTwo.astChildren.l) { case List(lhsIdentifier: Identifier, rhsBlock: Block) =>
            lhsIdentifier.name shouldBe "foo@tmp-2"
            lhsIdentifier.code shouldBe "$foo@tmp-2"

            inside(rhsBlock.astChildren.l) {
              case List(arrayAssignCall: Call, indexAssignCall: Call, retIdentifier: Identifier) =>
                arrayAssignCall.name shouldBe Operators.assignment
                arrayAssignCall.code shouldBe "$foo@tmp-3 = array()"

                indexAssignCall.name shouldBe Operators.assignment
                indexAssignCall.code shouldBe "$foo@tmp-3[2] = $foo@tmp-0"

                retIdentifier.name shouldBe "foo@tmp-3"
                retIdentifier.code shouldBe "$foo@tmp-3"
            }
          }

          arrayPushCall.name shouldBe "array_push"
          arrayPushCall.code shouldBe "$xs[] = $foo@tmp-2"
          arrayPushCall.order shouldBe 3
          inside(arrayPushCall.argument.l) { case List(lhsIdentifier: Identifier, rhsIdentifier: Identifier) =>
            lhsIdentifier.name shouldBe "xs"
            lhsIdentifier.code shouldBe "$xs"

            rhsIdentifier.name shouldBe "foo@tmp-2"
            rhsIdentifier.code shouldBe "$foo@tmp-2"
          }

          retIden.name shouldBe "val"
          retIden.code shouldBe "$val"
          retIden.order shouldBe 4
      }
    }
  }

  "assignments using the multi array dimension fetch syntax with last dimension should be rewritten as multiple assignments and index assignment" in {
    val cpg = code("""<?php
        |function foo($val) {
        |  $xs[1][2][] = $val;
        |}
        |""".stripMargin)

    inside(cpg.method.name("foo").body.astChildren.l) { case List(block: Block) =>
      block.code shouldBe "$xs[1][2][] = $val"
      block.lineNumber shouldBe Some(3)

      inside(block.astChildren.isLocal.l) { case List(xs) =>
        xs.name shouldBe "xs"
        xs.code shouldBe "$xs"
      }

      inside(block.astChildren.not(_.isLocal).l) { case List(arrayPushCall: Call, retIden: Identifier) =>
        arrayPushCall.name shouldBe "array_push"
        arrayPushCall.code shouldBe "$xs[1][2][] = $val"
        arrayPushCall.order shouldBe 1
        inside(arrayPushCall.argument.l) { case List(indexAccess: Call, identifier: Identifier) =>
          indexAccess.name shouldBe Operators.indexAccess
          indexAccess.code shouldBe "$xs[1][2]"

          identifier.name shouldBe "val"
          identifier.code shouldBe "$val"
        }

        retIden.name shouldBe "val"
        retIden.code shouldBe "$val"
        retIden.order shouldBe 2
      }
    }
  }

  "assignments using the multi array dimension fetch syntax with no dimensionless access should be an indexAccess" in {
    val cpg = code("""<?php
        |function foo($val) {
        |  $xs[1][2] = $val;
        |}
        |""".stripMargin)

    inside(cpg.method.name("foo").body.astChildren.l) { case List(local: Local, assignment: Call) =>
      local.name shouldBe "xs"
      local.code shouldBe "$xs"

      assignment.name shouldBe Operators.assignment
      assignment.code shouldBe "$xs[1][2] = $val"

      inside(assignment.argument.l) { case List(indexAccess: Call, identifier: Identifier) =>
        identifier.name shouldBe "val"
        identifier.code shouldBe "$val"

        indexAccess.name shouldBe Operators.indexAccess
        indexAccess.code shouldBe "$xs[1][2]"
      }
    }
  }

  "assignments using the multi array dimension fetch syntax with first dimension should be rewritten as multiple assignments" in {
    val cpg = code("""<?php
        |function foo($val) {
        |  $xs[1][][2] = $val;
        |}
        |""".stripMargin)

    inside(cpg.method.name("foo").body.astChildren.l) { case List(block: Block) =>
      block.code shouldBe "$xs[1][][2] = $val"
      block.lineNumber shouldBe Some(3)

      inside(block.astChildren.isLocal.l) { case List(tmp0, tmp1, xs) =>
        tmp0.name shouldBe "foo@tmp-0"
        tmp0.code shouldBe "$foo@tmp-0"
        tmp1.name shouldBe "foo@tmp-1"
        tmp1.code shouldBe "$foo@tmp-1"
        xs.name shouldBe "xs"
        xs.code shouldBe "$xs"
      }

      inside(block.astChildren.not(_.isLocal).l) {
        case List(assignOne: Call, arrayPushCall: Call, retIden: Identifier) =>
          assignOne.name shouldBe Operators.assignment
          assignOne.code shouldBe "$foo@tmp-0 = array(2 => $val)"
          assignOne.order shouldBe 1
          inside(assignOne.astChildren.l) { case List(lhsIdentifier: Identifier, rhsBlock: Block) =>
            lhsIdentifier.name shouldBe "foo@tmp-0"
            lhsIdentifier.code shouldBe "$foo@tmp-0"

            inside(rhsBlock.astChildren.l) {
              case List(arrayAssignCall: Call, indexAssignCall: Call, retIdentifier: Identifier) =>
                arrayAssignCall.name shouldBe Operators.assignment
                arrayAssignCall.code shouldBe "$foo@tmp-1 = array()"

                indexAssignCall.name shouldBe Operators.assignment
                indexAssignCall.code shouldBe "$foo@tmp-1[2] = $val"

                retIdentifier.name shouldBe "foo@tmp-1"
                retIdentifier.code shouldBe "$foo@tmp-1"
            }
          }

          arrayPushCall.name shouldBe "array_push"
          arrayPushCall.code shouldBe "$xs[1][] = $foo@tmp-0"
          arrayPushCall.order shouldBe 2
          inside(arrayPushCall.argument.l) { case List(indexAccess: Call, identifier: Identifier) =>
            indexAccess.name shouldBe Operators.indexAccess
            indexAccess.code shouldBe "$xs[1]"

            identifier.name shouldBe "foo@tmp-0"
            identifier.code shouldBe "$foo@tmp-0"
          }

          retIden.name shouldBe "val"
          retIden.code shouldBe "$val"
          retIden.order shouldBe 3
      }
    }
  }

  "assignments using the multi array dimension fetch syntax with a complex source expression should use a tmp for the rhs" in {
    val cpg = code("""<?php
        |function foo($val) {
        |  $xs[1][][2] = bar($val);
        |}
        |""".stripMargin)

    inside(cpg.method.name("foo").body.astChildren.l) { case List(block: Block) =>
      block.code shouldBe "$xs[1][][2] = bar($val)"
      block.lineNumber shouldBe Some(3)

      inside(block.astChildren.isLocal.l) { case List(tmp0, tmp1, tmp2, xs) =>
        tmp0.name shouldBe "foo@tmp-0"
        tmp0.code shouldBe "$foo@tmp-0"
        tmp1.name shouldBe "foo@tmp-1"
        tmp1.code shouldBe "$foo@tmp-1"
        tmp2.name shouldBe "foo@tmp-2"
        tmp2.code shouldBe "$foo@tmp-2"
        xs.name shouldBe "xs"
        xs.code shouldBe "$xs"
      }

      inside(block.astChildren.not(_.isLocal).l) {
        case List(assignOne: Call, assignTwo: Call, arrayPushCall: Call, retIden: Identifier) =>
          assignOne.name shouldBe Operators.assignment
          assignOne.code shouldBe "$foo@tmp-0 = bar($val)"
          assignOne.order shouldBe 1
          inside(assignOne.argument.l) { case List(identifier: Identifier, call: Call) =>
            identifier.name shouldBe "foo@tmp-0"
            identifier.code shouldBe "$foo@tmp-0"

            call.name shouldBe "bar"
            call.code shouldBe "bar($val)"
          }

          assignTwo.name shouldBe Operators.assignment
          assignTwo.code shouldBe "$foo@tmp-1 = array(2 => $foo@tmp-0)"
          assignTwo.order shouldBe 2

          arrayPushCall.name shouldBe "array_push"
          arrayPushCall.code shouldBe "$xs[1][] = $foo@tmp-1"
          arrayPushCall.order shouldBe 3
          inside(arrayPushCall.argument.l) { case List(indexAccess: Call, identifier: Identifier) =>
            indexAccess.name shouldBe Operators.indexAccess
            indexAccess.code shouldBe "$xs[1]"

            identifier.name shouldBe "foo@tmp-1"
            identifier.code shouldBe "$foo@tmp-1"
          }

          retIden.name shouldBe "foo@tmp-0"
          retIden.code shouldBe "$foo@tmp-0"
          retIden.order shouldBe 4
      }
    }
  }

  "associative array definitions should be lowered with the correct assignments" in {
    val cpg = code("""<?php
        |array(
        |  "A" => 1,
        |  "B" => 2
        |);
        |""".stripMargin)

    inside(cpg.method.internal.body.astChildren.l) { case List(tmpLocal: Local, arrayBlock: Block) =>
      tmpLocal.name shouldBe "Test0.php:<global>@tmp-0"
      tmpLocal.code shouldBe "$Test0.php:<global>@tmp-0"

      inside(arrayBlock.astChildren.l) {
        case List(initAssign: Call, aAssign: Call, bAssign: Call, tmpIdent: Identifier) =>
          initAssign.code shouldBe "$Test0.php:<global>@tmp-0 = array()"
          initAssign.lineNumber shouldBe Some(2)

          aAssign.code shouldBe "$Test0.php:<global>@tmp-0[\"A\"] = 1"
          aAssign.lineNumber shouldBe Some(3)

          bAssign.code shouldBe "$Test0.php:<global>@tmp-0[\"B\"] = 2"
          bAssign.lineNumber shouldBe Some(4)

          tmpIdent.name shouldBe "Test0.php:<global>@tmp-0"
          tmpIdent.code shouldBe "$Test0.php:<global>@tmp-0"
          tmpIdent._localViaRefOut should contain(tmpLocal)
      }
    }
  }

  "arrays in blocks should contain unique tmp locals" in {
    // using "array(...)" as its lowering generates tmp variables
    val cpg = code("""<?php
        |function foo() {
        |  $arr = array(1, 2);
        |
        |  foreach ($arr as list($a, $b)) {
        |    echo $a, $b;
        |  }
        |}
        |""".stripMargin)

    val tmp0Identifiers = cpg.identifier.nameExact("foo@tmp-0").l
    tmp0Identifiers.flatMap(_.lineNumber).distinct shouldBe List(3)

    val tmp1Identifiers = cpg.identifier.nameExact("foo@iter_tmp-1").l
    tmp1Identifiers.flatMap(_.lineNumber).distinct shouldBe List(5)

    val tmp2Identifiers = cpg.identifier.nameExact("foo@tmp-2").l
    tmp2Identifiers.flatMap(_.lineNumber).distinct shouldBe List(5)
  }

  "non-associative array definitions should be lowered with the correct index accesses and assignments" in {
    val cpg = code("""<?php
        |array(
        |  "A",
        |  "B"
        |);
        |""".stripMargin)

    inside(cpg.method.internal.body.astChildren.l) { case List(tmpLocal: Local, arrayBlock: Block) =>
      tmpLocal.name shouldBe "Test0.php:<global>@tmp-0"
      tmpLocal.code shouldBe "$Test0.php:<global>@tmp-0"

      inside(arrayBlock.astChildren.l) {
        case List(initAssign: Call, aAssign: Call, bAssign: Call, tmpIdent: Identifier) =>
          initAssign.code shouldBe "$Test0.php:<global>@tmp-0 = array()"
          initAssign.lineNumber shouldBe Some(2)

          aAssign.code shouldBe "$Test0.php:<global>@tmp-0[0] = \"A\""
          aAssign.lineNumber shouldBe Some(3)

          bAssign.code shouldBe "$Test0.php:<global>@tmp-0[1] = \"B\""
          bAssign.lineNumber shouldBe Some(4)

          tmpIdent.name shouldBe "Test0.php:<global>@tmp-0"
          tmpIdent.code shouldBe "$Test0.php:<global>@tmp-0"
          tmpIdent._localViaRefOut should contain(tmpLocal)
      }
    }
  }

  "arrays with int-compatible indices should have them treated as ints" in {
    val cpg = code("""<?php
        |array(
        |  "2" => "A"
        |);
        |""".stripMargin)

    inside(cpg.method.internal.body.astChildren.l) { case List(tmpLocal: Local, arrayBlock: Block) =>
      tmpLocal.name shouldBe "Test0.php:<global>@tmp-0"
      tmpLocal.code shouldBe "$Test0.php:<global>@tmp-0"

      inside(arrayBlock.astChildren.l) { case List(initAssign: Call, assign: Call, tmpIdent: Identifier) =>
        initAssign.code shouldBe "$Test0.php:<global>@tmp-0 = array()"
        initAssign.lineNumber shouldBe Some(2)

        assign.code shouldBe "$Test0.php:<global>@tmp-0[2] = \"A\""
        inside(assign.argument.collectAll[Call].argument.l) { case List(array: Identifier, index: Literal) =>
          array.name shouldBe "Test0.php:<global>@tmp-0"
          array.code shouldBe "$Test0.php:<global>@tmp-0"

          index.code shouldBe "2"
          index.typeFullName shouldBe "int"
        }

        tmpIdent.name shouldBe "Test0.php:<global>@tmp-0"
        tmpIdent.code shouldBe "$Test0.php:<global>@tmp-0"
        tmpIdent._localViaRefOut should contain(tmpLocal)
      }
    }
  }

  "mixed associative array definitions should be represented with correct keys" in {
    val cpg = code("""<?php
        |array(
        |  "A" => "B",
        |  "C",
        |  4 => "D",
        |  "E",
        |  "10" => "F",
        |  "G",
        |  8 => "H",
        |);
        |""".stripMargin)

    inside(cpg.method.internal.body.astChildren.l) { case List(tmpLocal: Local, arrayBlock: Block) =>
      tmpLocal.name shouldBe "Test0.php:<global>@tmp-0"
      tmpLocal.code shouldBe "$Test0.php:<global>@tmp-0"

      inside(arrayBlock.astChildren.l) {
        case List(
              initAssign: Call,
              aAssign: Call,
              cAssign: Call,
              fourAssign: Call,
              eAssign: Call,
              tenAssign: Call,
              gAssign: Call,
              eightAssign: Call,
              tmpIdent: Identifier
            ) =>
          initAssign.code shouldBe "$Test0.php:<global>@tmp-0 = array()"
          initAssign.lineNumber shouldBe Some(2)

          aAssign.code shouldBe "$Test0.php:<global>@tmp-0[\"A\"] = \"B\""
          cAssign.code shouldBe "$Test0.php:<global>@tmp-0[0] = \"C\""
          fourAssign.code shouldBe "$Test0.php:<global>@tmp-0[4] = \"D\""
          eAssign.code shouldBe "$Test0.php:<global>@tmp-0[5] = \"E\""
          tenAssign.code shouldBe "$Test0.php:<global>@tmp-0[10] = \"F\""
          gAssign.code shouldBe "$Test0.php:<global>@tmp-0[11] = \"G\""
          eightAssign.code shouldBe "$Test0.php:<global>@tmp-0[8] = \"H\""

          tmpIdent.name shouldBe "Test0.php:<global>@tmp-0"
          tmpIdent.code shouldBe "$Test0.php:<global>@tmp-0"
          tmpIdent._localViaRefOut should contain(tmpLocal)
      }
    }
  }
}
