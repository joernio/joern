require 'test_helper'
require 'shellwords'

describe Slop::Parser do
  before do
    @options = Slop::Options.new
    @verbose = @options.bool "-v", "--verbose"
    @name    = @options.string "-n", "--name"
    @unused  = @options.string "--unused"
    @parser  = Slop::Parser.new(@options)
    @result  = @parser.parse %w(foo -v --name lee argument)
  end

  it "ignores everything after --" do
    @parser.parse %w(-v -- -v --name lee)
    assert_equal [@verbose], @parser.used_options
    assert_equal ["-v", "--name", "lee"], @parser.arguments
  end

  describe "for flag=argument" do
    it "parses names and values" do
      @options.integer "-p", "--port"
      @result.parser.parse %w(--name=bob -p=123)
      assert_equal "bob", @result[:name]
      assert_equal 123, @result[:port]
    end

    it "treats an = separated by a space as a value" do
      @options.string "--foo"
      @result.parser.parse %w(--foo = =)
      assert_equal "=", @result[:foo]
      assert_equal %w(=), @result.args
    end

    it "includes = in strings" do
      @result.parser.parse(%w(--name=b=b))
      assert_equal "b=b", @result[:name]
    end
  end

  it "parses flag=''" do
    @options.string "--str"
    @options.array "--arr", default: ["array"]
    @result.parser.parse %(--str="" --arr="").shellsplit

    assert_equal "", @result[:str]
    assert_equal [], @result[:arr]
  end

  it "parses arg with leading -" do
    @options.string "-t", "--text"
    @result.parser.parse %w(--name=bob --text --sometext)
    assert_equal "bob", @result[:name]
    assert_equal "--sometext", @result[:text]
  end

  it "parses regexp arg with leading -" do
    @options.regexp "--pattern"
    @result.parser.parse %w(--pattern -x)
    assert_equal(/-x/, @result[:pattern])
  end

  it "parses negative integer" do
    @options.integer "-p", "--port"
    @result.parser.parse %w(--name=bob --port -123)
    assert_equal "bob", @result[:name]
    assert_equal(-123, @result[:port])
  end

  it "parses negative float" do
    @options.float "-m", "--multiple"
    @result.parser.parse %w(--name=bob -m -123.987)
    assert_equal "bob", @result[:name]
    assert_equal(-123.987, @result[:multiple])
  end

  describe "parsing grouped short flags" do
    before do
      @options.bool "-q", "--quiet"
    end

    it "parses boolean flags" do
      @result.parser.parse %w(-qv)
      assert_equal true, @result.quiet?
      assert_equal true, @result.verbose?
    end

    it "sends the argument to the last flag" do
      @result.parser.parse %w(-qvn foo)
      assert_equal "foo", @result[:name]
    end

    it "doesn't screw up single hyphen long options" do
      @options.string "-host"
      @result.parser.parse %w(-host localhost)
      assert_equal "localhost", @result[:host]
    end
  end

  describe "short flags with arguments" do
    before do
      @options.integer "-i"
      @options.string "-s"
    end

    it "parses the argument" do
      @result.parser.parse %w(-i5 -sfoo)
      assert_equal 5, @result[:i]
      assert_equal "foo", @result[:s]
    end
  end

  describe "#used_options" do
    it "returns all options that were parsed" do
      assert_equal [@verbose, @name], @parser.used_options
    end
  end

  describe "#unused_options" do
    it "returns all options that were not parsed" do
      assert_equal [@unused], @parser.unused_options
    end
  end

  describe "#arguments" do
    it "returns all unparsed arguments" do
      assert_equal %w(foo argument), @parser.arguments
    end

    it "does not return --" do
      @parser.parse %w(-v -- --name lee)
      assert_equal %w(--name lee), @parser.arguments
    end

    it "correctly removes the option argument" do
      @parser.parse %w(lee --name lee lee)
      assert_equal %w(lee lee), @parser.arguments
    end

    it "correctly removes options that use =" do
      @parser.parse %w(lee --name=lee lee)
      assert_equal %w(lee lee), @parser.arguments
    end
  end
end
