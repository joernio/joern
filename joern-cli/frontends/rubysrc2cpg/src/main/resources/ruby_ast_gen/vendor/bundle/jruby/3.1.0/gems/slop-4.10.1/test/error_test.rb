require 'test_helper'

# All raised errors tested here

describe Slop::MissingArgument do
  it "raises when an argument is missing" do
    opts = Slop::Options.new
    opts.string "-n", "--name"
    assert_raises(Slop::MissingArgument) { opts.parse %w(--name) }

    #Assert returns the argument question
    begin
      opts.parse %w(--name)
    rescue Slop::MissingArgument => e
      assert_equal(e.flags, ["-n", "--name"])
    end
  end

  it "does not raise when errors are suppressed" do
    opts = Slop::Options.new(suppress_errors: true)
    opts.string "-n", "--name"
    opts.parse %w(--name)
  end

  it "does not raise if '--' appears as the first argument" do
    opts = Slop::Options.new
    opts.string "-n", "--name"
    opts.parse %w(-- --name)
  end
end

describe Slop::UnknownOption do
  it "raises when an option is unknown" do
    opts = Slop::Options.new
    opts.string "-n", "--name"
    assert_raises(Slop::UnknownOption) { opts.parse %w(--foo) }

    #Assert returns the unknown option in question
    begin
      opts.parse %w(--foo)
    rescue Slop::UnknownOption => e
      assert_equal(e.flag, "--foo")
    end
  end

  it "does not raise when errors are suppressed" do
    opts = Slop::Options.new(suppress_errors: true)
    opts.string "-n", "--name"
    opts.parse %w(--foo)
  end
end

describe Slop::MissingRequiredOption do
  it "raises when a required option is missing" do
    opts = Slop::Options.new
    opts.string "-n", "--name", required: true
    assert_raises(Slop::MissingRequiredOption) { opts.parse [] }
  end

  it "does not raise when errors are suppressed" do
    opts = Slop::Options.new(suppress_errors: true)
    opts.string "-n", "--name", required: true
    opts.parse []
  end
end

describe Slop::InvalidOptionValue do
  it "raises when an option has an invalid value" do
    opts = Slop::Options.new(validate_types: true)
    opts.integer "-n", "--number", default: 10
    assert_raises(Slop::InvalidOptionValue) { opts.parse %w(-n foo) }
  end

  it "does not raise when errors are suppressed" do
    opts = Slop::Options.new(suppress_errors: true)
    opts.integer "-n", "--number", default: 10, validate_type: true
    r = opts.parse %w(-n foo)
    assert_equal(10, r[:n])
  end
end
