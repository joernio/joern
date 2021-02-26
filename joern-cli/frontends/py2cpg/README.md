Idea integration:
  - In order to use IntelliJ IDEA you must run `mill mill.scalalib.GenIdea/idea`.
    After that you can open (not import from existing source) the project in IDEA.
  - Whenever the build.sc file changes `mill mill.scalalib.GenIdea/idea` must be
    run. After that IDEA automatically picks up the changes.
    
Stage and run:
  - Build and assemble py2cpg for local execution: mill _.assembly
  - Run: ./py2cpg.sh <args>
       
Shortcomings of Python CPG representation:
  - No named parameter support
  - Incorrect instance argument for call like x.func.
    See source code comment.
  - No handling of __getattr__, __setattr__, etc.   
    
    
