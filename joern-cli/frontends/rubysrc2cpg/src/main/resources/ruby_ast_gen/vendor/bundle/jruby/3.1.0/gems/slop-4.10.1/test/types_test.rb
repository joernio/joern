require 'test_helper'

describe Slop::StringOption do
  before do
    @options = Slop::Options.new
    @age     = @options.string "--name"
    @minus   = @options.string "--zipcode"
    @result  = @options.parse %w(--name Foo --zipcode 12345)
  end

  it "returns the value as a string" do
    assert_equal "Foo", @result[:name]
    assert_equal "12345", @result[:zipcode]
  end
end

describe Slop::SymbolOption do
  before do
    @options = Slop::Options.new
    @age     = @options.symbol "--name"
    @minus   = @options.symbol "--zipcode"
    @result  = @options.parse %w(--name Foo --zipcode 12345)
  end

  it "returns the value as a symbol" do
    assert_equal :Foo, @result[:name]
    assert_equal :'12345', @result[:zipcode]
  end
end

describe Slop::BoolOption do
  before do
    @options  = Slop::Options.new
    @verbose  = @options.bool "--verbose", validate_type: true
    @quiet    = @options.bool "--quiet"
    @inversed = @options.bool "--inversed", default: true
    @explicit = @options.bool "--explicit", validate_type: true
    @bloc     = @options.bool("--bloc"){|val| (@bloc_val ||= []) << val}
    @result   = @options.parse %w(--verbose --no-inversed
                                  --bloc --no-bloc
                                  --explicit=false)
  end

  it "returns true if used" do
    assert_equal true, @result[:verbose]
  end

  it "returns false if not used" do
    assert_equal false, @result[:quiet]
  end

  it "can be inversed via --no- prefix" do
    assert_equal false, @result[:inversed]
  end

  it "will invert the value passed to &block via --no- prefix" do
    assert_equal [true, false], @bloc_val
  end

  it "returns false when explicitly false" do
    assert_equal false, @result[:explicit]
  end

  it "raises with invalid types" do
    assert_raises(Slop::InvalidOptionValue) do
      @result.parser.parse %w(--verbose foo)
    end
  end

  # Like above but without validate_type
  it "returns true if used and ignores the value" do
    @result.parser.parse %w(--quiet foo)

    assert_equal true, @result[:quiet]
  end
end

describe Slop::IntegerOption do
  before do
    @options = Slop::Options.new
    @age     = @options.integer "--age"
    @minus   = @options.integer "--minus", validate_type: true
    @plus    = @options.integer "--plus"
    @result  = @options.parse %w(--age 20 --minus -10 --plus +30)
  end

  it "returns the value as an integer" do
    assert_equal 20, @result[:age]
    assert_equal (-10), @result[:minus]
    assert_equal 30, @result[:plus]
  end

  it "returns nil for non-numbers by default" do
    @result.parser.parse %w(--age hello)
    assert_nil @result[:age]
  end

  it "raises with invalid types" do
    assert_raises(Slop::InvalidOptionValue) do
      @result.parser.parse %w(--minus foo)
    end
  end
end

describe Slop::FloatOption do
  before do
    @options = Slop::Options.new
    @apr     = @options.float "--apr"
    @apr_value = 2.9
    @minus = @options.float "--minus", validate_type: true
    @plus = @options.float "--plus"
    @scientific_notation = @options.float "--scientific-notation"
    @scientific_notation_value = 4e21
    @result  = @options.parse %W(--apr #{@apr_value} --minus -6.1 --plus +9.4 --scientific-notation #{@scientific_notation_value})
  end

  it "returns the value as a float" do
    assert_equal @apr_value, @result[:apr]
    assert_equal (-6.1), @result[:minus]
    assert_equal 9.4, @result[:plus]
  end

  it "parses scientific notations" do
    assert_equal @scientific_notation_value, @result[:scientific_notation]

    @scientific_notation_value = 4E21
    @result  = @options.parse %W(--scientific-notation #{@scientific_notation_value})
    assert_equal @scientific_notation_value, @result[:scientific_notation]

    @scientific_notation_value = 4.0e21
    @result  = @options.parse %W(--scientific-notation #{@scientific_notation_value})
    assert_equal @scientific_notation_value, @result[:scientific_notation]

    @scientific_notation_value = -4e21
    @result  = @options.parse %W(--scientific-notation #{@scientific_notation_value})
    assert_equal @scientific_notation_value, @result[:scientific_notation]

    @scientific_notation_value = 4e-21
    @result  = @options.parse %W(--scientific-notation #{@scientific_notation_value})
    assert_equal @scientific_notation_value, @result[:scientific_notation]
  end

  it "returns nil for non-numbers by default" do
    @result.parser.parse %w(--apr hello)
    assert_nil @result[:apr]
  end

  it "raises with invalid types" do
    assert_raises(Slop::InvalidOptionValue) do
      @result.parser.parse %w(--minus foo)
    end
  end
end

describe Slop::ArrayOption do
  before do
    @options = Slop::Options.new
    @files   = @options.array "--files"
    @multi   = @options.array "-M", delimiter: nil
    @delim   = @options.array "-d", delimiter: ":"
    @limit   = @options.array "-l", limit: 2
    @result  = @options.parse %w(--files foo.txt,bar.rb)
  end

  it "defaults to []" do
    assert_equal [], @result[:d]
  end

  it "parses comma separated args" do
    assert_equal %w(foo.txt bar.rb), @result[:files]
  end

  it "collects multiple option values" do
    @result.parser.parse %w(--files foo.txt --files bar.rb)
    assert_equal %w(foo.txt bar.rb), @result[:files]
  end

  it "collects multiple option values with no delimiter" do
    @result.parser.parse %w(-M foo,bar -M bar,qux)
    assert_equal %w(foo,bar bar,qux), @result[:M]
  end

  it "can use a custom delimiter" do
    @result.parser.parse %w(-d foo.txt:bar.rb)
    assert_equal %w(foo.txt bar.rb), @result[:d]
  end

  it "can use a custom limit" do
    @result.parser.parse %w(-l foo,bar,baz)
    assert_equal ["foo", "bar,baz"], @result[:l]
  end
end

describe Slop::NullOption do
  before do
    @options = Slop::Options.new
    @version = @options.null('--version')
    @result  = @options.parse %w(--version)
  end

  it 'has a return value of true' do
    assert_equal true, @result[:version]
  end

  it 'is not included in to_hash' do
    assert_equal({}, @result.to_hash)
  end
end

describe Slop::RegexpOption do
  before do
    @options       = Slop::Options.new
    @exclude       = @options.regexp "--exclude"
    @exclude_value = "redirect|news"
    @result        = @options.parse %W(--exclude #{@exclude_value})
  end

  it "returns the value as a Regexp" do
    assert_equal Regexp.new(@exclude_value), @result[:exclude]
  end
end
