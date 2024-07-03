package io.joern.kotlin2cpg.validation

import io.joern.kotlin2cpg.testfixtures.KotlinCode2CpgFixture
import io.joern.x2cpg.Defines
import io.shiftleft.codepropertygraph.generated.nodes.{Call, Method}
import io.shiftleft.semanticcpg.language.*

class MissingTypeInformationTests extends KotlinCode2CpgFixture(withOssDataflow = false) {
  "CPG for code with CALL to Java stdlib fn with argument of unknown type" should {
    lazy val cpg = code("""
        |import no.such.package.CommandMaker
        |fun fetchDailyDaveArticle(maker: CommandMaker) {
        |   val cmd = maker.make("curl -s https://seclists.org/dailydave/2021/q4/0")
        |   Runtime.getRuntime().exec(cmd)
        |}
        |""".stripMargin)

    "contain a CALL node with the correct METHOD_FULL_NAME set" in {
      val List(c: Call) = cpg.call.codeExact("Runtime.getRuntime().exec(cmd)").l
      c.methodFullName shouldBe s"java.lang.Runtime.exec:${Defines.UnresolvedSignature}(1)"
    }
  }

  "CPG for code with CALL to Kotlin stdlib fn with argument of unknown type" should {
    lazy val cpg = code("""
      |import no.such.package.UrlStringFixer
      |fun bibleurlprint(fixer: UrlStringFixer) {
      |   val fixedUrl = fixer.fix("https://pocorgtfo.hacke.rs/pocorgtfo00.pdf")
      |   println(fixedUrl)
      |}
      |""".stripMargin)

    "contain a CALL node with the correct METHOD_FULL_NAME set" in {
      val List(c: Call) = cpg.call.codeExact("println(fixedUrl)").l
      c.methodFullName shouldBe s"kotlin.io.println:${Defines.UnresolvedSignature}(1)"
    }
  }

  "CPG for code with class with constructor with a parameter missing type information" should {
    lazy val cpg = code("""
        |package com.insecureshop
        |
        |import android.view.LayoutInflater
        |import android.view.ViewGroup
        |import androidx.recyclerview.widget.RecyclerView
        |import com.bumptech.glide.Glide
        |import com.insecureshop.databinding.CartItemBinding
        |
        |class CartAdapter : RecyclerView.Adapter<CartAdapter.CartViewHolder>() {
        |    class CartViewHolder(binding: CartItemBinding) : RecyclerView.ViewHolder(binding.root) {
        |        var mBinding = binding
        |    }
        |
        |    override fun onCreateViewHolder(parent: ViewGroup, viewType: Int): CartViewHolder {
        |        val binding = CartItemBinding.inflate(LayoutInflater.from(parent.context))
        |        return CartViewHolder(binding)
        |    }
        |}
        |""".stripMargin)

    "contain METHODs node for the constructors with the METHOD_FULL_NAMEs set" in {
      val List(m1: Method, m2: Method, m3: Method) = cpg.method.nameExact("<init>").l
      m1.fullName shouldBe "com.insecureshop.CartAdapter.<init>:void()"
      m2.fullName shouldBe "com.insecureshop.CartAdapter.CartViewHolder.<init>:void(com.insecureshop.databinding.CartItemBinding)"
      m3.fullName shouldBe "com.insecureshop.CartAdapter.CartViewHolder.<init>:void(ANY)"
    }
  }
}
