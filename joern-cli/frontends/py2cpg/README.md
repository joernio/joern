Idea integration:
  - In order to use IntelliJ IDEA you must run `mill mill.scalalib.GenIdea/idea`.
    After that you can open (not import from existing source) the project in IDEA.
  - Whenever the build.sc file changes `mill mill.scalalib.GenIdea/idea` must be
    run. After that IDEA automatically picks up the changes.
