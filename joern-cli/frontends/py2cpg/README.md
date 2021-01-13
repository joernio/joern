Idea integration:
  - In order to use IntelliJ IDEA you must run `mill mill.scalalib.GenIdea/idea`.
    After that you can open (not import from existing source) the project in IDEA.
  - Whenever the build.sc file changes `mill mill.scalalib.GenIdea/idea` must be
    run. After that IDEA automatically picks up the changes.

PyDev artifact deployment:
  - We use PyDev as a dependency for parsing python code.
    Sadly PyDev is not published to a public artifactory and thus we need hand
    publish the artifacts to jfrog:
  - 1. Download the newest PyDev plugin zip from
       https://sourceforge.net/projects/pydev/files/
    2. From the zip extract the parser, core and shared_core jars.
       They are located under plugins/org.python.pydev.<jarName><version>/<jarName>.jar
    3. Login to jfrog under shiftleft.jfrog.io.
    4. For each of the 3 extracted jars go to Artifactory->Artifacts->Deploy
       (currently the Deploy button is on the top-right corner of the jfrog UI.
    4.1 Select libs-release-local as target repository.
    4.2 Drop jar into jfrog UI under "Single".
    4.3 As group ID enter `org.python.pydev`.
    4.4 As artifact ID enter parser, core and shared_core respectively.
    4.5 As version enter the current version of PyDev. At the time of
        writing this is way 8.1.0. Please stick to the schema.
    4.6 Select the "Generate Default POM" checkbox.
    4.7 Click deploy.
