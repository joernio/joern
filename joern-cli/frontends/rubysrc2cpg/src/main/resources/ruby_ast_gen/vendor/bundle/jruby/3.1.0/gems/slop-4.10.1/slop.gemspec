$:.unshift './lib'
require 'slop'

Gem::Specification.new do |s|
  s.name        = 'slop'
  s.version     = Slop::VERSION
  s.summary     = 'Simple Lightweight Option Parsing'
  s.description = 'A DSL for gathering options and parsing command line flags'
  s.author      = 'Lee Jarvis'
  s.email       = 'ljjarvis@gmail.com'
  s.homepage    = 'http://github.com/leejarvis/slop'
  s.files       = `git ls-files`.split("\n")
  s.license     = 'MIT'

  s.required_ruby_version = '>= 2.0.0'

  s.add_development_dependency 'rake'
  s.add_development_dependency 'minitest', '~> 5.0.0'
end
