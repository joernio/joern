# rubysrc2cpg

A `parser` Gem based parser for Ruby source code that creates code property graphs according to the specification at https://github.com/ShiftLeftSecurity/codepropertygraph .

The `parser` Gem is wrapped around a Ruby application [ruby_ast_gen](https://github.com/joernio/ruby_ast_gen) that is
then embedded under `src/main/resources` and executed during runtime using JRuby.

To update this, set the version under `src/main/resources/application.conf` and run `sbt rubysrc2cpg/astGenDlTask`.