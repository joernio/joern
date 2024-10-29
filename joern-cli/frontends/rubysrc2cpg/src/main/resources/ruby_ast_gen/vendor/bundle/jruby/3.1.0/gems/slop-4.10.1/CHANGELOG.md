Changelog
=========

v4.10.1 (2023-02-26)
--------------------

Bug fixes
  * Fix boolean flags from being invalid when followed by arguments #279

v4.10.0 (2023-02-15)
--------------------

Features:
  * Add support for optional type validation [#278](https://github.com/leejarvis/slop/pull/278) (Victor Gama)

v4.9.3 (2022-09-30)
-------------------

Bug fixes:
  * Fix explicitly false boolean options and allow for additional false arguments [#276](https://github.com/leejarvis/slop/pull/276) (Eugene Otto)

v4.9.2 (2022-03-26)
-------------------

Bug fixes:
  * Handle flag arguments that contain equals character [#275](https://github.com/leejarvis/slop/pull/275) (ConnorWGarvey)

v4.9.1 (2021-05-28)
-------------------

Bug fixes:
  * Fixed a bug where `flag=arg` syntax would raise an error when an
    empty value was passed. [#266](https://github.com/leejarvis/slop/issues/266)

v4.9.0 (2021-05-11)
-------------------

Features:
  * Add SymbolOption [#263](https://github.com/leejarvis/slop/pull/263)

Bug fixes:
  * Use `+=` over `<<` to handle frozen string literals. [255](https://github.com/leejarvis/slop/pull/255)

v4.8.2 (2020-07-10)
-------------------

Bug fixes:
  * Fix bug where separator position was messed up if using `help: false`
    [#253](https://github.com/leejarvis/slop/issues/253)

v4.8.1 (2020-03-31)
-------------------

Bug fixes:
  * Fix keyword argument warning. [#251](https://github.com/leejarvis/slop/pull/251)


v4.8.0 (2020-01-17)
-------------------

Features:
  * Add support for prefixing integer values with `+` character
  [#243](https://github.com/leejarvis/slop/pull/243) (Juha Ylitalo)
  * Add support for parsing floats with scientific notation
  [#250](https://github.com/leejarvis/slop/pull/250) (Hansuk Hong)

Maintenance:
  * Add 2.7.0 to CI and fix warnings
  [#248](https://github.com/leejarvis/slop/pull/248) (Juha Ylitalo, Andrew Kane)

v4.7.0 (2019-06-29)
-------------------

Features:
  * Add `Slop::Result#fetch`. It returns the value of given option, or raises an error if given option is not present. [#232](https://github.com/leejarvis/slop/pull/232) ([Giovanni Benussi](https://github.com/giovannibenussi))
  * Adding a separator without passing any arguments now creates a separator with the empty string. [#238](https://github.com/leejarvis/slop/pull/238) ([Teemu Matilainen](https://github.com/tmatilai))

Bug fixes
  * Ensure non-string option types have their flags consumed properly [#241] (Sutou Kouhei)


v4.6.2 (2018-03-12)
-------------------

Bug fixes/Enhancements
  * Fix equals character (=) being parsed incorrectly in some cases. [#226](https://github.com/leejarvis/slop/issues/226)

v4.6.1 (2017-11-20)
-------------------

Bug fixes/Enhancements
  * Fix separator so it doesn't mutate user data. [#223](https://github.com/leejarvis/slop/issues/223) (Marc-André Lafortune)
  * Add additional tests for `Options#separator` and fix issue where
    the last separator was ignored. [#222](https://github.com/leejarvis/slop/issues/222)

v4.6.0 (2017-10-06)
-------------------

Features
  * Add support for required options. [#218](https://github.com/leejarvis/slop/issues/218) (William Woodruff)

v4.5.0 (2017-05-22)
-------------------

Features:
  * Added config option to avoid translating flags-with-dashes into
  underscores. [#206](https://github.com/leejarvis/slop/issues/206) (@lbriais)

v4.4.3 (2017-05-02)
-------------------

Bug fixes:
  * Ruby 2.0.0 support broken in v4.4.2

v4.4.2 (2017-04-29)
-------------------

Bug fixes:
  * Fix support for parsing -x5 or -nfoo. [#199](https://github.com/leejarvis/slop/issues/199)
  * Fix removing arguments after `--`. [#194](https://github.com/leejarvis/slop/issues/194)

v4.4.1 (2016-08-21)
-------------------

Bug fixes:
  * Handle bad constant names in `Slop.option_defined?`. [#198](https://github.com/leejarvis/slop/issues/198)
    (Ellen Marie Dash)

v4.4.0 (2016-08-15)
-------------------

Features
  * Support parsing arguments prefixed with dashes. [#192](https://github.com/leejarvis/slop/issues/192) (Andrew Clemons)

Bug fixes:
  * Retain sort order inside tail sort. [#193](https://github.com/leejarvis/slop/issues/193) (Caio Chassot)

v4.3.0 (2016-03-19)
-------------------

Features
  * Allow disabling array delimiter. [#189](https://github.com/leejarvis/slop/issues/189) (Mike Pastore)
  * Allow passing custom banner as config. [#191](https://github.com/leejarvis/slop/issues/191) (Philip Rees)

v4.2.1 (2015-11-25)
-------------------

Features:
  * Better handling of option names with multiple words. [#169](https://github.com/leejarvis/slop/issues/169) (Tim Rogers)

Minor enhancements:
  * add ARGF notes to Arguments (README). [#173](https://github.com/leejarvis/slop/issues/173) (Rick Hull)

Bug fixes:
  * Fix arguments removed with option arguments. [#182](https://github.com/leejarvis/slop/issues/182) (Naoki Mizuno)
  * Fix bug where true is passed to BoolOption block regardless
    of --no- prefix. [#184](https://github.com/leejarvis/slop/issues/184) (Ben Brady)
  * only raise MissingArgument if not `default_value`. [#163](https://github.com/leejarvis/slop/issues/163) (Ben Brady)

v4.2.0 (2015-04-18)
-------------------

Features:
  * Support for Regexp option type [#167](https://github.com/leejarvis/slop/issues/167) (Laurent Arnoud)
  * Support prefixed `--no-` for explicitly setting boolean options
    to `false` [#168](https://github.com/leejarvis/slop/issues/168)
  * Better handling of flags with multiple words [#169](https://github.com/leejarvis/slop/issues/169) (Tim Rogers)

v4.1.0 (2015-04-18)
-------------------

Features:
  * Support for FloatOption [#156](https://github.com/leejarvis/slop/issues/156) (Rick Hull)
  * Support for `limit` config to ArrayOption.
  * Support for `tail` config to add options to the bottom of
    the help text.
  * Add explicit setter (#[]=) to Result class. [#162](https://github.com/leejarvis/slop/issues/162)
  * Implement flag gettings for UnknownOption and MissingArgument
    error classes. [#165](https://github.com/leejarvis/slop/issues/165) (sigurdsvela)

Minor enhancements:
  * Reset parser every time `parse` is called.

Bug fixes:
  * Remove "--" from unprocessed arguments [#157](https://github.com/leejarvis/slop/issues/157) (David Rodríguez).

v4.0.0 (2014-12-27)
-------------------

Features:
  * Rebuilt from the ground up. See the v3 changelog for all existing
    changes: https://github.com/leejarvis/slop/blob/v3/CHANGES.md
