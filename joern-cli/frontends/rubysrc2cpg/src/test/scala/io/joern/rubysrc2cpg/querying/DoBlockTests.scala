package io.joern.rubysrc2cpg.querying

import io.joern.rubysrc2cpg.testfixtures.RubyCode2CpgFixture

class DoBlockTests extends RubyCode2CpgFixture {

  "a basic parameterized do-block with braces" should {
    val cpg = code(
      """
        |my_array = [1, 2, 3]
        |my_array.each { |item|
        |    puts item
        |}
        |""".stripMargin)

    "create an anonymous method with associated type declaration" in {

    }

    "have specify the `item` parameter in the method" in {

    }
  }

  "a basic parameterized do-block with pipes" should {

    val cpg = code(
      """
        |my_array = [:uno, :dos, :tres]
        |my_array.each do |item|
        |    puts item
        |end
        |""".stripMargin)

    "create an anonymous method with associated type declaration" in {

    }

    "have specify the `item` parameter in the method" in {

    }

  }

  "a do block iterating over a hash" should {

    val cpg = code(
      """
        |hash = { "a" => 1, "b" => 2 }
        |hash.each do |key, value|
        |  puts "#{key}-----"
        |  puts value
        |end
        |""".stripMargin)

    "create an anonymous method with associated type declaration" in {

    }

    "have specify the `key` and `value` parameters in the method" in {

    }

  }

}
