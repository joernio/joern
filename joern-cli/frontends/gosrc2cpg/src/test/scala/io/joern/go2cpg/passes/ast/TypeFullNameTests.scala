package io.joern.go2cpg.passes.ast

import io.joern.go2cpg.testfixtures.GoCodeToCpgSuite
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.semanticcpg.language.*

import java.io.File
import scala.collection.immutable.List

class TypeFullNameTests extends GoCodeToCpgSuite {
  "Type check for declared primitive types" should {
    val cpg = code("""
        |package main
        |func main() {
        |   var a int = 1
        |   var b, c float32
        |   var d []int
        |}
        |""".stripMargin)

    "Check for local nodes" in {
      val List(a, b, c, d) = cpg.local.l
      a.typeFullName shouldBe "int"
      b.typeFullName shouldBe "float32"
      c.typeFullName shouldBe "float32"
      d.typeFullName shouldBe "[]int"
    }

    "check for identifier nodes" in {
      val List(a, b, c, d) = cpg.identifier.l
      a.typeFullName shouldBe "int"
      b.typeFullName shouldBe "float32"
      c.typeFullName shouldBe "float32"
      d.typeFullName shouldBe "[]int"
    }
  }

  "Type check for implicit Type based on assigned literal" should {
    val cpg = code("""
        |package main
        |func main() {
        |   var a = 10
        |   var b = 20.5
        |   var c = "Somestr"
        |   var d = true
        |   var e = false
        |   var f = [5]int{1,2}
        |}
        |""".stripMargin)

    "check for LITERAL nodes types" in {
      val List(a, b, c, d, e, _, _) = cpg.literal.l
      a.typeFullName shouldBe "int"
      b.typeFullName shouldBe "float32"
      c.typeFullName shouldBe "string"
      d.typeFullName shouldBe "bool"
      e.typeFullName shouldBe "bool"
    }

    "check for array call nodes types" in {
      val List(arrayInitialiser) = cpg.call.nameNot(Operators.assignment).l
      arrayInitialiser.typeFullName shouldBe "[]int"
    }

    "Check for local nodes" in {
      val List(a, b, c, d, e, f) = cpg.local.l
      a.typeFullName shouldBe "int"
      b.typeFullName shouldBe "float32"
      c.typeFullName shouldBe "string"
      d.typeFullName shouldBe "bool"
      e.typeFullName shouldBe "bool"
      f.typeFullName shouldBe "[]int"
    }

    "check for identifier nodes" in {
      val List(a, b, c, d, e, f) = cpg.identifier.l
      a.typeFullName shouldBe "int"
      b.typeFullName shouldBe "float32"
      c.typeFullName shouldBe "string"
      d.typeFullName shouldBe "bool"
      e.typeFullName shouldBe "bool"
      f.typeFullName shouldBe "[]int"
    }
  }

  "Type check for Operator call nodes" should {
    val cpg = code("""
        |package main
        |func main() {
        |	x := 10
        |	y := 5
        |   xx := "Pandurang"
        |   yy := "Patil"
        |   xxx := true
        |   yyy := false
        |   var a = "Pandurang" + "Patil"
        |   var b = xx + yy
        |   c := xx + yy
        |   var d = 20 + 30
        |   var e = x + y
        |   f := x + y
        |   var g = 40 - 50
        |   var h = x - y
        |   i := x - y
        |   var j = 20 * 30
        |   var k = x * y
        |   l := x * y
        |   var m = 50 / 2
        |   var n = x / y
        |   o := x / y
        |   var p = 50 % 2
        |   var q = x % y
        |   r := x % y
        |   var s = x == y
        |   t := xx == yy
        |   var u = x != y
        |   v := xx != yy
        |   var w = x > y
        |   z := x > y
        |   var aa = x < y
        |   bb := x < y
        |   var cc = x >= y
        |   dd := x >= y
        |   var ee = x <= y
        |   ff := x <= y
        |   var gg = xxx && yyy
        |   hh := xxx && yyy
        |   var ii = xxx || yyy
        |   jj := xxx || yyy
        |   var kk = 10 & 100
        |   var ll = x & y
        |   mm := x & y
        |   var nn = 10 | 100
        |   var oo = x | y
        |   pp := x | y
        |   var qq = 10 ^ 100
        |   var rr = x ^ y
        |   ss := x ^ y
        |   var tt = 10 << 100
        |   var uu = x << y
        |   vv := x << y
        |   var aaa = 10 >> 100
        |   var bbb = x >> y
        |   ccc := x >> y
        |}
        |""".stripMargin)

    "Check for local nodes" in {
      val List(
        x,
        y,
        xx,
        yy,
        xxx,
        yyy,
        a,
        b,
        c,
        d,
        e,
        f,
        g,
        h,
        i,
        j,
        k,
        l,
        m,
        n,
        o,
        p,
        q,
        r,
        s,
        t,
        u,
        v,
        w,
        z,
        aa,
        bb,
        cc,
        dd,
        ee,
        ff,
        gg,
        hh,
        ii,
        jj,
        kk,
        ll,
        mm,
        nn,
        oo,
        pp,
        qq,
        rr,
        ss,
        tt,
        uu,
        vv,
        aaa,
        bbb,
        ccc
      ) =
        cpg.local.l
      x.typeFullName shouldBe "int"
      y.typeFullName shouldBe "int"
      xx.typeFullName shouldBe "string"
      yy.typeFullName shouldBe "string"
      xxx.typeFullName shouldBe "bool"
      yyy.typeFullName shouldBe "bool"
      aaa.typeFullName shouldBe "int"
      bbb.typeFullName shouldBe "int"
      ccc.typeFullName shouldBe "int"
      tt.typeFullName shouldBe "int"
      uu.typeFullName shouldBe "int"
      vv.typeFullName shouldBe "int"
      qq.typeFullName shouldBe "int"
      rr.typeFullName shouldBe "int"
      ss.typeFullName shouldBe "int"
      nn.typeFullName shouldBe "int"
      oo.typeFullName shouldBe "int"
      pp.typeFullName shouldBe "int"
      kk.typeFullName shouldBe "int"
      ll.typeFullName shouldBe "int"
      mm.typeFullName shouldBe "int"
      ii.typeFullName shouldBe "bool"
      jj.typeFullName shouldBe "bool"
      gg.typeFullName shouldBe "bool"
      hh.typeFullName shouldBe "bool"
      ee.typeFullName shouldBe "bool"
      ff.typeFullName shouldBe "bool"
      cc.typeFullName shouldBe "bool"
      dd.typeFullName shouldBe "bool"
      aa.typeFullName shouldBe "bool"
      bb.typeFullName shouldBe "bool"
      w.typeFullName shouldBe "bool"
      z.typeFullName shouldBe "bool"
      u.typeFullName shouldBe "bool"
      v.typeFullName shouldBe "bool"
      s.typeFullName shouldBe "bool"
      t.typeFullName shouldBe "bool"
      p.typeFullName shouldBe "int"
      q.typeFullName shouldBe "int"
      r.typeFullName shouldBe "int"
      m.typeFullName shouldBe "int"
      n.typeFullName shouldBe "int"
      o.typeFullName shouldBe "int"
      j.typeFullName shouldBe "int"
      k.typeFullName shouldBe "int"
      l.typeFullName shouldBe "int"
      g.typeFullName shouldBe "int"
      h.typeFullName shouldBe "int"
      i.typeFullName shouldBe "int"
      f.typeFullName shouldBe "int"
      e.typeFullName shouldBe "int"
      d.typeFullName shouldBe "int"
      c.typeFullName shouldBe "string"
      b.typeFullName shouldBe "string"
      a.typeFullName shouldBe "string"
    }

    "check for identifier nodes" in {
      val List(x) = cpg.identifier("x").typeFullName.dedup.l
      x shouldBe "int"
      val List(y) = cpg.identifier("y").typeFullName.dedup.l
      y shouldBe "int"
      val List(xx) = cpg.identifier("xx").typeFullName.dedup.l
      xx shouldBe "string"
      val List(yy) = cpg.identifier("yy").typeFullName.dedup.l
      yy shouldBe "string"

      val List(aaa, bbb, ccc) = cpg.identifier("aaa|bbb|ccc").l
      aaa.typeFullName shouldBe "int"
      bbb.typeFullName shouldBe "int"
      ccc.typeFullName shouldBe "int"

      val List(tt, uu, vv) = cpg.identifier("tt|uu|vv").l
      tt.typeFullName shouldBe "int"
      uu.typeFullName shouldBe "int"
      vv.typeFullName shouldBe "int"

      val List(qq, rr, ss) = cpg.identifier("qq|rr|ss").l
      qq.typeFullName shouldBe "int"
      rr.typeFullName shouldBe "int"
      ss.typeFullName shouldBe "int"

      val List(nn, oo, pp) = cpg.identifier("nn|oo|pp").l
      nn.typeFullName shouldBe "int"
      oo.typeFullName shouldBe "int"
      pp.typeFullName shouldBe "int"

      val List(kk, ll, mm) = cpg.identifier("kk|ll|mm").l
      kk.typeFullName shouldBe "int"
      ll.typeFullName shouldBe "int"
      mm.typeFullName shouldBe "int"

      val List(ii, jj) = cpg.identifier("ii|jj").l
      ii.typeFullName shouldBe "bool"
      jj.typeFullName shouldBe "bool"

      val List(gg, hh) = cpg.identifier("gg|hh").l
      gg.typeFullName shouldBe "bool"
      hh.typeFullName shouldBe "bool"

      val List(ee, ff) = cpg.identifier("ee|ff").l
      ee.typeFullName shouldBe "bool"
      ff.typeFullName shouldBe "bool"

      val List(cc, dd) = cpg.identifier("cc|dd").l
      cc.typeFullName shouldBe "bool"
      dd.typeFullName shouldBe "bool"

      val List(aa, bb) = cpg.identifier("aa|bb").l
      aa.typeFullName shouldBe "bool"
      bb.typeFullName shouldBe "bool"

      val List(w, z) = cpg.identifier("[w|z]").l
      w.typeFullName shouldBe "bool"
      z.typeFullName shouldBe "bool"

      val List(u, v) = cpg.identifier("[u|v]").l
      u.typeFullName shouldBe "bool"
      v.typeFullName shouldBe "bool"

      val List(s, t) = cpg.identifier("[s|t]").l
      s.typeFullName shouldBe "bool"
      t.typeFullName shouldBe "bool"

      val List(p, q, r) = cpg.identifier("[p|q|r]").l
      p.typeFullName shouldBe "int"
      q.typeFullName shouldBe "int"
      r.typeFullName shouldBe "int"

      val List(m, n, o) = cpg.identifier("[m|n|o]").l
      m.typeFullName shouldBe "int"
      n.typeFullName shouldBe "int"
      o.typeFullName shouldBe "int"

      val List(j, k, l) = cpg.identifier("[j|k|l]").l
      j.typeFullName shouldBe "int"
      k.typeFullName shouldBe "int"
      l.typeFullName shouldBe "int"

      val List(g, h, i) = cpg.identifier("[g|h|i]").l
      g.typeFullName shouldBe "int"
      h.typeFullName shouldBe "int"
      i.typeFullName shouldBe "int"

      val List(d, e, f) = cpg.identifier("[d|e|f]").l
      d.typeFullName shouldBe "int"
      e.typeFullName shouldBe "int"
      f.typeFullName shouldBe "int"

      val List(a, b, c) = cpg.identifier("[a|b|c]").l
      c.typeFullName shouldBe "string"
      b.typeFullName shouldBe "string"
      a.typeFullName shouldBe "string"

    }

    "Check call nodes for + operator type full name" in {
      val List(a, b, c, d, e, f) = cpg.call(Operators.addition).l
      d.typeFullName shouldBe "int"
      e.typeFullName shouldBe "int"
      f.typeFullName shouldBe "int"
      c.typeFullName shouldBe "string"
      b.typeFullName shouldBe "string"
      a.typeFullName shouldBe "string"
    }

    "check call nodes for - operator type full name" in {
      val List(g, h, i) = cpg.call(Operators.subtraction).l
      g.typeFullName shouldBe "int"
      h.typeFullName shouldBe "int"
      i.typeFullName shouldBe "int"
    }

    "check call nodes for * operator type full name" in {
      val List(j, k, l) = cpg.call(Operators.multiplication).l
      j.typeFullName shouldBe "int"
      k.typeFullName shouldBe "int"
      l.typeFullName shouldBe "int"
    }

    "check call nodes for / operator type full name" in {
      val List(m, n, o) = cpg.call(Operators.division).l
      m.typeFullName shouldBe "int"
      n.typeFullName shouldBe "int"
      o.typeFullName shouldBe "int"
    }

    "check call nodes for % operator type full name" in {
      val List(p, q, r) = cpg.call(Operators.modulo).l
      p.typeFullName shouldBe "int"
      q.typeFullName shouldBe "int"
      r.typeFullName shouldBe "int"
    }

    "check call nodes for == operator type full name" in {
      val List(s, t) = cpg.call(Operators.equals).l
      s.typeFullName shouldBe "bool"
      t.typeFullName shouldBe "bool"
    }

    "check call nodes for != operator type full name" in {
      val List(u, v) = cpg.call(Operators.notEquals).l
      u.typeFullName shouldBe "bool"
      v.typeFullName shouldBe "bool"
    }

    "check call nodes for > operator type full name" in {
      val List(w, z) = cpg.call(Operators.greaterThan).l
      w.typeFullName shouldBe "bool"
      z.typeFullName shouldBe "bool"
    }

    "check call nodes for < operator type full name" in {
      val List(aa, bb) = cpg.call(Operators.lessThan).l
      aa.typeFullName shouldBe "bool"
      bb.typeFullName shouldBe "bool"
    }

    "check call nodes for >= operator type full name" in {
      val List(cc, dd) = cpg.call(Operators.greaterEqualsThan).l
      cc.typeFullName shouldBe "bool"
      dd.typeFullName shouldBe "bool"
    }

    "check call nodes for <= operator type full name" in {
      val List(ee, ff) = cpg.call(Operators.lessEqualsThan).l
      ee.typeFullName shouldBe "bool"
      ff.typeFullName shouldBe "bool"
    }

    "check call nodes for && operator type full name" in {
      val List(gg, hh) = cpg.call(Operators.logicalAnd).l
      gg.typeFullName shouldBe "bool"
      hh.typeFullName shouldBe "bool"
    }

    "check call nodes for || operator type full name" in {
      val List(ii, jj) = cpg.call(Operators.logicalOr).l
      ii.typeFullName shouldBe "bool"
      jj.typeFullName shouldBe "bool"
    }

    "check call nodes for & operator type full name" in {
      val List(kk, ll, mm) = cpg.call(Operators.and).l
      kk.typeFullName shouldBe "int"
      ll.typeFullName shouldBe "int"
      mm.typeFullName shouldBe "int"
    }

    "check call nodes for | operator type full name" in {
      val List(nn, oo, pp) = cpg.call(Operators.or).l
      nn.typeFullName shouldBe "int"
      oo.typeFullName shouldBe "int"
      pp.typeFullName shouldBe "int"
    }

    "check call nodes for ^ operator type full name" in {
      val List(qq, rr, ss) = cpg.call(Operators.xor).l
      qq.typeFullName shouldBe "int"
      rr.typeFullName shouldBe "int"
      ss.typeFullName shouldBe "int"
    }

    "check call nodes for << operator type full name" in {
      val List(tt, uu, vv) = cpg.call(Operators.shiftLeft).l
      tt.typeFullName shouldBe "int"
      uu.typeFullName shouldBe "int"
      vv.typeFullName shouldBe "int"
    }

    "check call nodes for >> operator type full name" in {
      val List(aaa, bbb, ccc) = cpg.call(Operators.arithmeticShiftRight).l
      aaa.typeFullName shouldBe "int"
      bbb.typeFullName shouldBe "int"
      ccc.typeFullName shouldBe "int"
    }
  }

  "Type check for += call nodes with string arguments " should {
    val cpg = code("""
        |package main
        |func main() {
        |   x := "pandurang"
        |	y := "patil"
        |   var z = "somestr"
        |	x += "p"
        |   y += x
        |   z += "test"
        |}""".stripMargin)

    "Check for local nodes" in {
      val List(typefullname) = cpg.local.typeFullName.dedup.l
      typefullname shouldBe "string"
    }

    "check for identifier nodes" in {
      val List(typefullname) = cpg.identifier.typeFullName.dedup.l
      typefullname shouldBe "string"
    }

    "Operator += call node type check" in {
      val List(a, b, c) = cpg.call(Operators.assignmentPlus).l
      a.typeFullName shouldBe "string"
      b.typeFullName shouldBe "string"
      c.typeFullName shouldBe "string"
    }
  }

  "Type check for += call nodes with float arguments " should {
    val cpg = code("""
        |package main
        |func main() {
        |   x := 10.6
        |	y := 20.7
        |   var z = 46.5
        |	x += 34.6
        |   y += x
        |   z += 20.5
        |}""".stripMargin)

    "Check for local nodes" in {
      val List(typefullname) = cpg.local.typeFullName.dedup.l
      typefullname shouldBe "float32"
    }

    "check for identifier nodes" in {
      val List(typefullname) = cpg.identifier.typeFullName.dedup.l
      typefullname shouldBe "float32"
    }

    "Operator += call node type check" in {
      val List(a, b, c) = cpg.call(Operators.assignmentPlus).l
      a.typeFullName shouldBe "float32"
      b.typeFullName shouldBe "float32"
      c.typeFullName shouldBe "float32"
    }
  }

  "Type check for += call nodes with int arguments " should {
    val cpg = code("""
        |package main
        |func main() {
        |   x := 10
        |	y := 20
        |   var z = 30
        |	x += 100
        |   y += x
        |   z += 200
        |}""".stripMargin)

    "Check for local nodes" in {
      val List(typefullname) = cpg.local.typeFullName.dedup.l
      typefullname shouldBe "int"
    }

    "check for identifier nodes" in {
      val List(typefullname) = cpg.identifier.typeFullName.dedup.l
      typefullname shouldBe "int"
    }

    "Operator += call node type check" in {
      val List(a, b, c) = cpg.call(Operators.assignmentPlus).l
      a.typeFullName shouldBe "int"
      b.typeFullName shouldBe "int"
      c.typeFullName shouldBe "int"
    }
  }

  "Type Check for Arrays" should {
    val cpg = code("""
        |package main
        |func main() {
        |   var a [][]string = [][]string{{"1", "2"}, {"3", "4"}}
        |   b := a[0][1]
        |   var c = [][]string{{"1", "2"}, {"3", "4"}}
        |   var d = c[1]
        |   var e = d[0]
        |   var f = c[0][0]
        |   var g []int = []int{1,2,3}
        |   var h = g[0]
        |   i := g
        |   j := []float32{ 1.2, 2.5}
        |   var k = i[0]
        |   var l = j[0]
        |}
        |""".stripMargin)
    "Type Check LOCAL nodes working" in {
      val List(a, b, c, d, e, f, g, h, i, j, k, l) = cpg.local.l
      a.typeFullName shouldBe "[][]string"
      g.typeFullName shouldBe "[]int"
      b.typeFullName shouldBe "string"
      c.typeFullName shouldBe "[][]string"
      d.typeFullName shouldBe "[]string"
      e.typeFullName shouldBe "string"
      f.typeFullName shouldBe "string"
      h.typeFullName shouldBe "int"
      i.typeFullName shouldBe "[]int"
      j.typeFullName shouldBe "[]float32"
      k.typeFullName shouldBe "int"
      l.typeFullName shouldBe "float32"
    }

    "Type check for CALL nodes working" in {
      val List(a, b, c, d, e, f, g, h, i, j, k, l, m) = cpg.call.nameNot(Operators.assignment).l
      a.typeFullName shouldBe "[][]string"
      b.typeFullName shouldBe "string"
      c.typeFullName shouldBe "[]string"
      d.typeFullName shouldBe "[][]string"
      i.typeFullName shouldBe "[]int"
      j.typeFullName shouldBe "int"
      k.typeFullName shouldBe "[]float32"
      e.typeFullName shouldBe "[]string"
      f.typeFullName shouldBe "string"
      g.typeFullName shouldBe "string"
      h.typeFullName shouldBe "[]string"
      l.typeFullName shouldBe "int"
      m.typeFullName shouldBe "float32"
    }
  }

  "Method call return value assigned to variable type check" should {
    val cpg = code(
      """
        |module joern.io/sample
        |go 1.18
        |""".stripMargin,
      "go.mod"
    ).moreCode(
      """
        |package lib
        |type Person struct{
        |   fname string
        |   lname string
        |}
        |func (person Person) fullName() string {
        |	return person.fname + " " + person.lname
        |}
        |""".stripMargin,
      Seq("lib", "typelib.go").mkString(File.separator)
    ).moreCode(
      """
        |package fpkg
        |import "joern.io/sample/lib"
        |func bar() string{
        |  return "somestr"
        |}
        |func createPerson(fn, ln string) lib.Person {
        |  return lib.Person{fname: fn, lname: ln}
        |}
        |""".stripMargin,
      Seq("fpkg", "mainlib.go").mkString(File.separator)
    ).moreCode(
      """
        |package main
        |import "joern.io/sample/fpkg"
        |import "joern.io/sample/lib"
        |func foo() {
        |  var a = fpkg.bar()
        |  var per = fpkg.createPerson("Pandurang", "Patil")
        |  var compName = per.fullName()
        |  var perOne lib.Person = fpkg.createPerson("Sameer", "Shinde")
        |  var compNameOne = perOne.fullName()
        |  var perThree = lib.Person{fname: "Ram", lname: "Thakur"}
        |  perFour := lib.Person{fname: "Seema", lname: "Dubey"}
        |  b := perFour.fname
        |  c := perFour.lname
        |}
        |""".stripMargin,
      "main.go"
    )

    "Call node typeFullName check with primitive return type" in {
      val List(bar) = cpg.call("bar").l
      bar.typeFullName shouldBe "string"
    }

    "Call node typeFullName check with struct return type" in {
      val List(createPerson) = cpg.call("createPerson").lineNumber(7).l
      createPerson.typeFullName shouldBe "joern.io/sample/lib.Person"
    }

    "Call node typeFullName check for function call on receiver object usecase 1" in {
      val List(fullName) = cpg.call("fullName").lineNumber(8).l
      fullName.typeFullName shouldBe "string"
    }

    "Call node typeFullName check for function call on receiver object usecase 2" in {
      val List(fullName) = cpg.call("fullName").lineNumber(10).l
      fullName.typeFullName shouldBe "string"
    }

    "Variable type checks" in {
      val List(a) = cpg.local("a").l
      a.typeFullName shouldBe "string"

      val List(aident) = cpg.identifier("a").lineNumber(6).l
      aident.typeFullName shouldBe "string"

      val List(pident) = cpg.identifier("per").lineNumber(7).l
      pident.typeFullName shouldBe "joern.io/sample/lib.Person"

      val List(compName) = cpg.identifier("compName").lineNumber(8).l
      compName.typeFullName shouldBe "string"

      val List(compNameOne) = cpg.identifier("compNameOne").lineNumber(10).l
      compNameOne.typeFullName shouldBe "string"

      val List(perThree) = cpg.identifier("perThree").lineNumber(11).l
      perThree.typeFullName shouldBe "joern.io/sample/lib.Person"

      val List(perFour) = cpg.identifier("perFour").lineNumber(12).l
      perFour.typeFullName shouldBe "joern.io/sample/lib.Person"

      val List(perOne) = cpg.identifier("perOne").lineNumber(9).l
      perOne.typeFullName shouldBe "joern.io/sample/lib.Person"
    }

    "Field access CALL node type check" in {
      val List(a, b, c, d) = cpg.call(Operators.fieldAccess).l
      a.typeFullName shouldBe "string"
      b.typeFullName shouldBe "string"
      c.typeFullName shouldBe "string"
      d.typeFullName shouldBe "string"
    }
  }

  "Struct Type used in another struct type check" should {
    val cpg = code(
      """
        |module joern.io/sample
        |go 1.18
        |""".stripMargin,
      "go.mod"
    ).moreCode(
      """
          |package lib
          |
          |type Address struct {
          |	addone string
          |	addtwo string
          |}
          |type Person struct {
          |	name     string
          |	age      int
          |	location string
          |	address  Address
          |}
          |
          |""".stripMargin,
      Seq("lib", "typelib.go").mkString(File.separator)
    ).moreCode(
      """
        |package main
        |import "joern.io/sample/lib"
        |func main() {
        |	var a = lib.Address{}
        |	var p = lib.Person{"pandurang", 10, "", a}
        |	var name = p.name
        |	address := p.address
        |}
        |""".stripMargin,
      "main.go"
    )

    "Check CALL nodes of RHS" in {
      val List(a, b, c, d) = cpg.call.nameNot(Operators.assignment).l
      a.typeFullName shouldBe "joern.io/sample/lib.Address"
      b.typeFullName shouldBe "joern.io/sample/lib.Person"
      c.typeFullName shouldBe "string"
      d.typeFullName shouldBe "joern.io/sample/lib.Address"
    }

    "Check LHS LOCAL Nodes for types" in {
      val List(a, b, c, d) = cpg.local.l
      a.typeFullName shouldBe "joern.io/sample/lib.Address"
      b.typeFullName shouldBe "joern.io/sample/lib.Person"
      c.typeFullName shouldBe "string"
      d.typeFullName shouldBe "joern.io/sample/lib.Address"
    }
  }

  "Struct type used before defining it" should {
    val cpg = code("""
        |package main
        |func main() {
        |  var p = Person{name: "Pandurang"}
        |  var c = p.name
        |}
        |type Person struct{
        |  name string
        |}
        |""".stripMargin)
    "LOCAL Node type check" in {
      val List(p, c) = cpg.local.l
      p.typeFullName shouldBe "main.Person"
      c.typeFullName shouldBe "string"
    }

    "CALL Node type check" in {
      val List(a, b) = cpg.call.nameNot(Operators.assignment).l
      a.typeFullName shouldBe "main.Person"
      b.typeFullName shouldBe "string"
    }
  }

  "Use case where namespace folder is not matching with declared package name" should {
    val cpg = code(
      """
        |module joern.io/sample
        |go 1.18
        |""".stripMargin,
      "go.mod"
    ).moreCode(
      """
        |package fpkg
        |type Sample struct {
        |  Name string
        |}
        |func Woo(a int) int{
        |   return 0
        |}
        |""".stripMargin,
      Seq("lib", "lib.go").mkString(File.separator)
    ).moreCode(
      """
        |package main
        |import "joern.io/sample/lib"
        |func main() {
        |  var a = fpkg.Woo(10)
        |  var b = fpkg.Sample{name: "Pandurang"}
        |  var c = b.Name
        |  var d fpkg.Sample
        |}
        |""".stripMargin,
      "main.go"
    )

    "Check METHOD Node" in {
      cpg.method("Woo").size shouldBe 1
      val List(x) = cpg.method("Woo").l
      x.fullName shouldBe "joern.io/sample/lib.Woo"
      x.signature shouldBe "joern.io/sample/lib.Woo(int)int"
    }

    "Check CALL Node" in {
      val List(x) = cpg.call("Woo").l
      x.methodFullName shouldBe "joern.io/sample/lib.Woo"
      x.typeFullName shouldBe "int"
    }

    "Traversal from call to callee method node" in {
      val List(x) = cpg.call("Woo").callee.l
      x.fullName shouldBe "joern.io/sample/lib.Woo"
      x.isExternal shouldBe false
    }

    "Check TypeDecl Node" in {
      val List(x) = cpg.typeDecl("Sample").l
      x.fullName shouldBe "joern.io/sample/lib.Sample"
    }

    "Check LOCAL Nodes" in {
      val List(a, b, c, d) = cpg.local.l
      a.typeFullName shouldBe "int"
      b.typeFullName shouldBe "joern.io/sample/lib.Sample"
      c.typeFullName shouldBe "string"
      d.typeFullName shouldBe "joern.io/sample/lib.Sample"
    }
  }
}
