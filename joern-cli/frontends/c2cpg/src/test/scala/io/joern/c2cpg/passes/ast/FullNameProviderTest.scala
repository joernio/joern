package io.joern.c2cpg.passes.ast

import io.joern.c2cpg.astcreation.FullNameProvider
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class FullNameProviderTest extends AnyWordSpec with Matchers {

  import FullNameProvider.stripTemplateTags

  "FullNameProvider.stripTemplateTags" should {

    "work for basic templates" in {
      stripTemplateTags("std.vector<int>") shouldBe "std.vector"
      stripTemplateTags("std.map<std.string, int>") shouldBe "std.map"
      stripTemplateTags("Template<T>") shouldBe "Template"
    }

    "preserve special tags" in {
      stripTemplateTags("<lambda>1") shouldBe "<lambda>1"
      stripTemplateTags("Foo.<lambda>1") shouldBe "Foo.<lambda>1"
      stripTemplateTags("Class<T>.<anonymous>1") shouldBe "Class.<anonymous>1"
      stripTemplateTags("Class<T>.<iterator>") shouldBe "Class.<iterator>"
      stripTemplateTags("Class<T>.foo(int)<const>") shouldBe "Class.foo(int)<const>"
    }

    "handle nested templates" in {
      stripTemplateTags("std.map<std.string, std.vector<int>>") shouldBe "std.map"
      stripTemplateTags("std.function<int(std.vector<int>)>") shouldBe "std.function"
      stripTemplateTags("Foo<std.vector<int>>.bar") shouldBe "Foo.bar"
      stripTemplateTags("Foo<std.vector<int>>.bar<Bar<int>>") shouldBe "Foo.bar"
      stripTemplateTags("Foo<std.vector<int>>.bar<Bar<int>>()<const>") shouldBe "Foo.bar()<const>"
      stripTemplateTags(
        "Foo<std.vector<int>>.bar<Bar<int>>.<lambda>1:string(Param<int>,Param<Bar<int>>)"
      ) shouldBe "Foo.bar.<lambda>1:string(Param,Param)"
    }

    "handle edge cases" in {
      stripTemplateTags("") shouldBe ""
      stripTemplateTags("NoTemplate") shouldBe "NoTemplate"
      stripTemplateTags("<") shouldBe "<"
      stripTemplateTags(">") shouldBe ">"
      stripTemplateTags("<>") shouldBe ""
    }

  }

}
