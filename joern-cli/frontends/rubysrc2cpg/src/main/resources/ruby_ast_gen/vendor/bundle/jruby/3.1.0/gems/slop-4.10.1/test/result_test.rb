require 'test_helper'

module Slop
  class ReverseEverythingOption < BoolOption
    def finish(result)
      result.used_options.grep(Slop::StringOption).each do |opt|
        opt.value = opt.value.reverse
      end
    end
  end
end

describe Slop::Result do
  before do
    @options     = Slop::Options.new
    @verbose     = @options.bool "-v", "--verbose"
    @name        = @options.string "--name"
    @unused      = @options.string "--unused"
    @long_option = @options.string "--long-option"
    @result      = @options.parse %w(foo -v --name lee --long-option bar argument)
  end

  it "increments option count" do
    # test this here so it's more "full stack"
    assert_equal 1, @verbose.count
    assert_equal 1, @long_option.count
    @result.parser.parse %w(-v --verbose)
    assert_equal 2, @verbose.count
  end

  it "handles default values" do
    @options.string("--foo", default: "bar")
    @result.parser.parse %w()
    assert_equal "bar", @result[:foo]

    @result.parser.parse %w(--foo)
    assert_equal "bar", @result[:foo]
  end

  it "handles custom finishing" do
    @options.string "--foo"
    @options.reverse_everything "-r"
    @result.parser.parse %w(-r --name lee --foo bar)
    assert_equal %w(eel rab), @result.to_hash.values_at(:name, :foo)
  end

  it "yields arguments to option blocks" do
    output = nil
    @options.string("--foo") { |v| output = v }
    @result.parser.parse %w(--foo bar)
    assert_equal output, "bar"
  end

  describe "#[]" do
    it "returns an options value" do
      assert_equal "lee", @result["name"]
      assert_equal "lee", @result[:name]
      assert_equal "lee", @result["--name"]
      assert_equal "bar", @result["long_option"]
      assert_equal "bar", @result[:long_option]
      assert_equal "bar", @result["--long-option"]
    end
  end

  describe "#fetch" do
    it "returns an options value" do
      assert_equal "lee", @result.fetch("--name")
    end

    it "raises Slop::UnknownOption when an option does not exists" do
      e = assert_raises(Slop::UnknownOption) { @result.fetch("--unexisting") }
      assert_equal "option not found: 'unexisting'", e.message
    end

    it "returns the default value of an option when a value is not provided" do
      @options.string("--foo", default: "bar")
      @result.parser.parse %w(--foo)

      assert_equal 'bar', @result.fetch('foo')
    end

    it "returns nil when an option is not provided and it does not have a default value" do
      @options.string("--hello")
      @result.parser.parse %w()

      assert_nil @result.fetch('hello')
    end
  end

  describe "#[]=" do
    it "sets an options value" do
      assert_equal "lee", @result["name"]
      @result["name"] = "bob"
      assert_equal "bob", @result[:name]
    end

    it "raises if an option isn't found" do
      assert_raises ArgumentError do
        @result["zomg"] = "something"
      end
    end
  end

  describe "#method_missing" do
    it "checks if options have been used" do
      assert_equal true, @result.verbose?
      assert_equal false, @result.unused?
      assert_equal true, @result.long_option?
    end
  end

  describe "#option" do
    it "returns an option by flag" do
      assert_equal @verbose, @result.option("--verbose")
      assert_equal @verbose, @result.option("-v")
      assert_equal @long_option, @result.option("--long-option")
    end

    it "ignores prefixed hyphens" do
      assert_equal @verbose, @result.option("verbose")
      assert_equal @verbose, @result.option("-v")
    end

    it "returns nil if nothing is found" do
      assert_nil @result.option("foo")
    end
  end

  describe "#to_hash" do
    it "returns option keys and values" do
      assert_equal({ verbose: true, name: "lee", unused: nil, long_option: "bar" },
                   @result.to_hash)
    end
  end
end
