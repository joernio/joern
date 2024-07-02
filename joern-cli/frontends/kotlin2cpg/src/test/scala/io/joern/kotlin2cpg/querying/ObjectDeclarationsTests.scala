package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.testfixtures.KotlinCode2CpgFixture
import io.shiftleft.semanticcpg.language.*

class ObjectDeclarationsTests extends KotlinCode2CpgFixture(withOssDataflow = false) {
  "CPG for code with simple object declaration" should {
    val cpg = code("""
        |package mypkg
        |
        |object Foo {
        |    val bar = "PLACEHOLDER_1"
        |    var baz = "PLACEHOLDER_2"
        |
        |    fun moo() = println("moo")
        |}
        |
        |fun main() {
        |  Foo.moo()
        |}
        |""".stripMargin)

    "should contain a TYPE_DECL node for the object declaration with the correct properties set" in {
      val List(x) = cpg.typeDecl.isExternal(false).name("Foo").l
      x.name shouldBe "Foo"
      x.code shouldBe "Foo"
      x.fullName shouldBe "mypkg.Foo"
      x.inheritsFromTypeFullName shouldBe List("java.lang.Object")
      x.isExternal shouldBe false
      x.lineNumber shouldBe Some(4)
      x.columnNumber shouldBe Some(7)
    }

    "should contain MEMBER node for `bar` with correct properties" in {
      val List(x) = cpg.member("bar").l
      x.name shouldBe "bar"
      x.code shouldBe "bar"
      x.typeFullName shouldBe "java.lang.String"
      x.lineNumber shouldBe Some(5)
      x.columnNumber shouldBe Some(8)
    }

    "should contain MEMBER node for `baz` with correct properties" in {
      val List(x) = cpg.member("baz").l
      x.name shouldBe "baz"
      x.code shouldBe "baz"
      x.typeFullName shouldBe "java.lang.String"
      x.lineNumber shouldBe Some(6)
      x.columnNumber shouldBe Some(8)
    }

    "should contain a CALL node for the call to `moo` with the correct properties set" in {
      val List(c) = cpg.call.code("Foo.moo.*").l
      c.methodFullName shouldBe "mypkg.Foo.moo:void()"
      c.typeFullName shouldBe "void"
    }
  }

  "CPG for code with complex object declaration" should {
    val cpg = code("""
        |package mypkg
        |
        |import android.content.Context
        |import android.content.SharedPreferences
        |
        |object Prefs {
        |    lateinit var sharedpreferences: SharedPreferences
        |    var prefs : Prefs? = null
        |
        |    fun getInstance(context: Context): Prefs {
        |        if (prefs == null) {
        |            sharedpreferences =
        |                context.getSharedPreferences("Prefs", Context.MODE_PRIVATE)
        |            prefs = this
        |        }
        |        return prefs!!
        |    }
        |
        |    var data: String?
        |        get() = sharedpreferences.getString("data","")
        |        set(value) {
        |            sharedpreferences.edit().putString("data", value).apply()
        |        }
        |
        |    var username: String?
        |        get() = sharedpreferences.getString("username","")
        |        set(value) {
        |            sharedpreferences.edit().putString("username", value).apply()
        |        }
        |
        |    var password: String?
        |        get() = sharedpreferences.getString("password","")
        |        set(value) {
        |            sharedpreferences.edit().putString("password", value).apply()
        |        }
        |
        |    var productList: String?
        |        get() = sharedpreferences.getString("productList","")
        |        set(value) {
        |            sharedpreferences.edit().putString("productList", value).apply()
        |        }
        |
        |    fun clearAll(){
        |        sharedpreferences.edit().clear().apply()
        |    }
        |}
        |
        |""".stripMargin)

    "should contain a TYPE_DECL node for the object declaration with the correct properties set" in {
      val List(x) = cpg.typeDecl.isExternal(false).name("Prefs").l
      x.name shouldBe "Prefs"
      x.fullName shouldBe "mypkg.Prefs"
      x.inheritsFromTypeFullName shouldBe List("java.lang.Object")
      x.isExternal shouldBe false
      x.lineNumber shouldBe Some(7)
      x.columnNumber shouldBe Some(7)
    }
  }
}
