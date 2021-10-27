package io.shiftleft.console.scripting

import com.google.protobuf.DescriptorProtos.FileDescriptorProto

import scala.reflect.runtime.currentMirror

/** a workaround for a scala reflect bug, where the first invocation of scala reflect fails
  * idea: invoke this at the very start to trigger the bug. future invocations should be fine
  * i asked to move the retry logic into scala reflect as a better way to handle this
  * https://github.com/scala/bug/issues/12038#issuecomment-760629585
  *
  * AMMENDMENT:
  * Actually doing this once is not always enough. Not sure why this is the case but if the fact
  * reflection has happened "long enough" before the intend reflections, the bug can again be
  * triggered. This was just hidden by the fact that the tests got executed in a forked process,
  * effectively always resetting the "already worked around" flag.
  * So we we now do this any time workaroundScalaReflectBugByTriggeringReflection() is called and
  * should be fine.
  * AND ALWAYS REMEMBER: Global state is the root of all evil.
  *
  * to reproduce the issue, comment out this workaround, publish cpg locally and run the following in the codescience repo:
  * `cpg2sp/testOnly *PolicyAmmoniteExecutorTest`
  * if the above is green without this workaround, we don't need it any more
  */
object ScalaReflectWorkaround {

  def fromJava(t: FileDescriptorProto): Unit = {
    println(t)
    // this is just here to suppress a warnings - it is never invoked by anything afaik,
    // but for whatever reason essential to trigger the scala reflection bug...
  }

  def workaroundScalaReflectBugByTriggeringReflection() = {
    try {
      currentMirror
        .reflectModule(currentMirror.staticModule("io.shiftleft.console.scripting.ScalaReflectWorkaround$"))
        .instance
    } catch {
      case _: Throwable => // that's what we want to trigger - it happens the first time, then works - all good
    }
  }
}
