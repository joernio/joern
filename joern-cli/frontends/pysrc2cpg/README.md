Stage and run:
  - Build and assemble pysrc2cpg for local execution: sbt pysrc2cpg/stage
  - Run: ./pysrc2cpg.sh <path/to/sourceDirOrFile> -o <path/to/outputCpg>

Shortcomings of Python CPG representation:
  - No named parameter support
  - Incorrect instance argument for call like x.func.
    See source code comment.
  - No handling of __getattr__, __setattr__, etc.   
