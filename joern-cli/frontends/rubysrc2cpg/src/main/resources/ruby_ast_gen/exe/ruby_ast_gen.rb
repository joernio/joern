#!/usr/bin/env ruby

# require "bundler/setup"
# We do the below instead of the above, as the above can be flaky
libs = File.expand_path("../../vendor/bundle/ruby/*/gems/**/lib", __FILE__)
$LOAD_PATH.unshift *Dir.glob(libs)

require "slop"

require_relative "../lib/ruby_ast_gen"

begin
  opts = Slop.parse do |o|
    o.string '-i', '--input', 'The input file or directory', required: true
    o.string '-o', '--output', 'The output directory', default: '.ast'
    o.string '-e', '--exclude', 'The exclusion regex', default: '^(tests?|vendor|spec)'
    o.string '-l', '--log', 'The logging level', default: 'warn'
    o.on '--version', 'Print the version' do
      puts RubyAstGen::VERSION
      exit
    end
    o.on '--help', 'Print usage' do
      puts o
      exit
    end
  end
rescue Slop::Error => e
  puts e.message
  exit 1 # Exit with an error code
end

RubyAstGen::parse(opts)
