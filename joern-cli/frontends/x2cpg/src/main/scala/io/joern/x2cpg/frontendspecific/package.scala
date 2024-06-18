package io.joern.x2cpg

/** This package solely exists to extract some code from the frontends that is shared between joern and the frontends,
  * e.g. for parsing commandline arguments and running postprocessing passes.
  *
  * If this code was to be in the frontend's subproject, joern would need to have classpath-dependencies on the
  * frontends, inheriting all their transitive dependencies. As discussed in e.g.
  * https://github.com/joernio/joern/issues/4625#issuecomment-2166427270 we want to avoid having classpath dependencies
  * on the frontends and instead invoke them frontends as external processes (i.e. execute their start script).
  * Otherwise we'll end in jar hell with various incompatible versions of many different dependencies, and complex
  * issues with things like OSGI and JPMS.
  */
package object frontendspecific
