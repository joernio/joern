require "test_helper"

describe Slop do
  describe ".parse" do
    it "parses a list of arguments" do
      result = Slop.parse(%w[--name Lee]) do |o|
        o.string "--name"
      end

      assert_equal "Lee", result[:name]
    end
  end

  describe ".option_defined?" do
    it "handles bad constant names" do
      assert_equal false, Slop.option_defined?("Foo?Bar")
    end

    it "returns false if the option is not defined" do
      assert_equal false, Slop.option_defined?("FooBar")
    end

    it "returns true if the option is defined" do
      assert_equal true, Slop.option_defined?("String")
    end
  end
end
