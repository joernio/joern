# frozen_string_literal: true

require_relative "lib/ruby_ast_gen/version"

Gem::Specification.new do |spec|
  spec.name = "ruby_ast_gen"
  spec.version = RubyAstGen::VERSION
  spec.authors = ["David Baker Effendi", "Andrei Dreyer"]
  spec.email = ["dave@whirlylabs.com", "andrei@whirlylabs.com"]

  spec.summary = "A Ruby parser than dumps the AST as JSON output"
  spec.description = "A Ruby parser than dumps the AST as JSON output for Joern's `rubysrc2cpg` frontend"
  spec.homepage = "https://github.com/whirlylabs/ruby_ast_gen"
  spec.license = "MIT"
  spec.required_ruby_version = ">= 3.0.7"

  spec.metadata["homepage_uri"] = spec.homepage

  # Specify which files should be added to the gem when it is released.
  # The `git ls-files -z` loads the files in the RubyGem that have been added into git.
  gemspec = File.basename(__FILE__)
  spec.files = Dir.glob("{lib,exe,sig,vendor}/**/*") +
    %w[README.md LICENSE.txt Rakefile Gemfile ruby_ast_gen.gemspec]
  spec.bindir = "exe"
  spec.executables = spec.files.grep(%r{\Aexe/}) { |f| File.basename(f) }
  spec.require_paths = ["lib"]
end
