require 'slop/option'
require 'slop/options'
require 'slop/parser'
require 'slop/result'
require 'slop/types'
require 'slop/error'

module Slop
  VERSION = '4.10.1'

  # Parse an array of options (defaults to ARGV). Accepts an
  # optional hash of configuration options and block.
  #
  # Example:
  #
  #   opts = Slop.parse(["-host", "localhost"]) do |o|
  #     o.string '-host', 'a hostname', default: '0.0.0.0'
  #   end
  #   opts.to_hash #=> { host: 'localhost' }
  #
  # Returns a Slop::Result.
  def self.parse(items = ARGV, **config, &block)
    Options.new(**config, &block).parse(items)
  end

  # Example:
  #
  #   Slop.option_defined?(:string) #=> true
  #   Slop.option_defined?(:omg)    #=> false
  #
  # Returns true if an option is defined.
  def self.option_defined?(name)
    const_defined?(string_to_option(name.to_s))
  rescue NameError
    # If a NameError is raised, it wasn't a valid constant name,
    # and thus couldn't have been defined.
    false
  end

  # Example:
  #
  #   Slop.string_to_option("string")     #=> "StringOption"
  #   Slop.string_to_option("some_thing") #=> "SomeThingOption"
  #
  # Returns a camel-cased class looking string with Option suffix.
  def self.string_to_option(s)
    s.gsub(/(?:^|_)([a-z])/) { $1.capitalize } + "Option"
  end

  # Example:
  #
  #   Slop.string_to_option_class("string") #=> Slop::StringOption
  #   Slop.string_to_option_class("foo")    #=> uninitialized constant FooOption
  #
  # Returns the full qualified option class. Uses `#string_to_option`.
  def self.string_to_option_class(s)
    const_get(string_to_option(s))
  end
end
