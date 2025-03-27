package io.joern.c2cpg.astcreation

import org.scalatest.funsuite.AnyFunSuiteLike

class FullNameProviderTest extends AnyFunSuiteLike {

  test("stripTemplateTags for basic templates") {
    assert(FullNameProvider.stripTemplateTags("std.vector<int>") == "std.vector")
    assert(FullNameProvider.stripTemplateTags("std.map<std.string, int>") == "std.map")
    assert(FullNameProvider.stripTemplateTags("Template<T>") == "Template")
  }

  test("stripTemplateTags preserves special tags") {
    assert(FullNameProvider.stripTemplateTags("<lambda>1") == "<lambda>1")
    assert(FullNameProvider.stripTemplateTags("Foo.<lambda>1") == "Foo.<lambda>1")
    assert(FullNameProvider.stripTemplateTags("Class<T>.<anonymous>1") == "Class.<anonymous>1")
    assert(FullNameProvider.stripTemplateTags("Class<T>.<iterator>") == "Class.<iterator>")
    assert(FullNameProvider.stripTemplateTags("Class<T>.<const>") == "Class.<const>")
  }

  test("stripTemplateTags handles nested templates") {
    assert(FullNameProvider.stripTemplateTags("std.map<std.string, std.vector<int>>") == "std.map")
    assert(FullNameProvider.stripTemplateTags("std.function<int(std.vector<int>)>") == "std.function")
  }

  test("stripTemplateTags handles edge cases") {
    assert(FullNameProvider.stripTemplateTags("") == "")
    assert(FullNameProvider.stripTemplateTags("NoTemplate") == "NoTemplate")
    assert(FullNameProvider.stripTemplateTags("<") == "<")
    assert(FullNameProvider.stripTemplateTags(">") == ">")
    assert(FullNameProvider.stripTemplateTags("<>") == "")
  }

}
