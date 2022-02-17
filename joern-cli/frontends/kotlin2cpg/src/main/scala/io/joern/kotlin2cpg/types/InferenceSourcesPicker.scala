package io.joern.kotlin2cpg.types

import better.files.{File => BFile}
import io.joern.kotlin2cpg.InferenceJarPath

object InferenceSourcesPicker {

  // In the following directory structure:
  //  ____________________
  //  | dir1
  //  |   -> build.gradle.kts
  //  |   -> dir2
  //  |      -> build.gradle.kts
  //  |      -> dir3
  //  |        -> source1.kt
  //  |        -> source2.kt
  //  |-------------------
  //  The list of paths which are acceptable for the current version of the Kotlin compiler API is:
  //  `Seq("dir1/dir2/dir3")` and nothing else.

  def dirsForRoot(rootDir: String): Seq[String] = {
    val dir = BFile(rootDir)

    // for an initial version, we only do very basic selection logic for `rootDir` without subdirs
    val hasSubDirs = dir.list.exists(_.isDirectory)
    if (!hasSubDirs) {
      return Seq(rootDir)
    }

    dir.listRecursively
      .filter(_.isDirectory)
      .flatMap { f =>
        val dirsPicked =
          f.list
            .filter(_.isDirectory)
            .filterNot { d =>
              d.listRecursively
                .filter(_.hasExtension)
                .exists(_.pathAsString.endsWith(".kts"))
            }
            .toList
            .map(_.pathAsString)
        if (dirsPicked.nonEmpty) {
          Some(dirsPicked)
        } else {
          None
        }
      }
      .flatten
      .toList
  }

  // TODO: iterate over inferencejars dir and get the paths like so
  val defaultInferenceJarPaths = Seq(
    InferenceJarPath("inferencejars/android-4.1.1.4.jar", isResource = true),
    InferenceJarPath("inferencejars/androidx.activity.activity-ktx-1.2.4.jar", isResource = true),
    InferenceJarPath("inferencejars/androidx.annotation-1.1.0.jar", isResource = true),
    InferenceJarPath("inferencejars/androidx.compose.foundation-1.0.5.jar", isResource = true),
    InferenceJarPath("inferencejars/androidx.compose.foundation-layout-1.0.5.jar", isResource = true),
    InferenceJarPath("inferencejars/androidx.constraintlayout-2.1.1.jar", isResource = true),
    InferenceJarPath("inferencejars/androidx.core-1.1.0.jar", isResource = true),
    InferenceJarPath("inferencejars/androidx.core-ktx-1.6.0.jar", isResource = true),
    InferenceJarPath("inferencejars/androidx.fragment-1.4.0.jar", isResource = true),
    InferenceJarPath("inferencejars/androidx.fragment-ktx-1.4.0.jar", isResource = true),
    InferenceJarPath("inferencejars/androidx.lifecycle-viewmodel-ktx-2.2.0.jar", isResource = true),
    InferenceJarPath("inferencejars/androidx.localbroadcastmanager-1.0.0.jar", isResource = true),
    InferenceJarPath("inferencejars/androidx.preference-ktx-1.1.1.jar", isResource = true),
    InferenceJarPath("inferencejars/androidx.viewpager.viewpager-1.0.0.jar", isResource = true),
    InferenceJarPath("inferencejars/androidx.recyclerview.recyclerview-1.0.0.jar", isResource = true),
    InferenceJarPath("inferencejars/androidx.webkit-1.4.0.jar", isResource = true),
    InferenceJarPath("inferencejars/annotation-1.1.0.jar", isResource = true),
    InferenceJarPath("inferencejars/appcompat-1.3.1-classes.jar", isResource = true),
    InferenceJarPath("inferencejars/com.appdynamics.appdynamics-runtime-20.7.1.jar", isResource = true),
    InferenceJarPath("inferencejars/com.chimerapps.niddler.niddler-1.5.5.jar", isResource = true),
    InferenceJarPath("inferencejars/com.chimerapps.niddler.niddler-noop-1.5.5.jar", isResource = true),
    InferenceJarPath("inferencejars/com.facebook.stetho.stetho-1.5.0.jar", isResource = true),
    InferenceJarPath("inferencejars/com.facebook.stetho.stetho-okhttp3-1.5.0.jar", isResource = true),
    InferenceJarPath("inferencejars/com.getkeepsafe.relinker.relinker-1.4.4.jar", isResource = true),
    InferenceJarPath("inferencejars/com.github.bumptech.glide.glide-4.9.0.jar", isResource = true),
    InferenceJarPath("inferencejars/com.android.support.webkit-28.0.0.jar", isResource = true),
    InferenceJarPath("inferencejars/com.google.android.material.material-1.4.0.jar", isResource = true),
    InferenceJarPath("inferencejars/com.google.dagger.dagger-2.38.1.jar", isResource = true),
    InferenceJarPath("inferencejars/com.google.dagger.dagger-android-2.38.1.jar", isResource = true),
    InferenceJarPath("inferencejars/com.google.dagger.dagger-android-support-2.38.1.jar", isResource = true),
    InferenceJarPath("inferencejars/com.google.firebase.firebase-crashlytics-17.2.1.jar", isResource = true),
    InferenceJarPath("inferencejars/com.google.guava.guava-27.1-android.jar", isResource = true),
    InferenceJarPath("inferencejars/com.jakewharton.timber.timber-4.5.1.jar", isResource = true),
    InferenceJarPath("inferencejars/commons-io.commons-io-2.6.jar", isResource = true),
    InferenceJarPath("inferencejars/com.squareup.okhttp3.logging-interceptor-4.8.0.jar", isResource = true),
    InferenceJarPath("inferencejars/cryptonode.jncryptor-1.2.0.jar", isResource = true),
    InferenceJarPath("inferencejars/dagger-compiler-2.38.1.jar", isResource = true),
    InferenceJarPath("inferencejars/google.zixing.core-3.2.0.jar", isResource = true),
    InferenceJarPath("inferencejars/gson-2.8.9.jar", isResource = true),
    InferenceJarPath("inferencejars/http4k-core-4.14.1.4.jar", isResource = true),
    InferenceJarPath("inferencejars/io.reactivex.rxjava2.rxandroid-2.1.0.jar", isResource = true),
    InferenceJarPath("inferencejars/javax.servlet-api-4.0.1.jar", isResource = true),
    InferenceJarPath("inferencejars/javalin-4.1.1.jar", isResource = true),
    InferenceJarPath("inferencejars/jncryptor-1.2.0.jar", isResource = true),
    InferenceJarPath("inferencejars/kotlin-android-extensions-runtime-1.6.0-M1.jar", isResource = true),
    InferenceJarPath("inferencejars/kotlin-stdlib-1.6.0.jar", isResource = true),
    InferenceJarPath("inferencejars/kotlin-stdlib-common-1.6.0.jar", isResource = true),
    InferenceJarPath("inferencejars/kotlin-stdlib-jdk8-1.6.0.jar", isResource = true),
    InferenceJarPath("inferencejars/org.apache.commons.commons-collections4-4.4.jar", isResource = true),
    InferenceJarPath("inferencejars/org.apache.commons.commons-lang3-3.10.jar", isResource = true),
    InferenceJarPath("inferencejars/org.apache.commons.commons-text-1.8.jar", isResource = true),
    InferenceJarPath("inferencejars/org.jetbrains.kotlin.kotlin-android-extensions-1.6.0.jar", isResource = true),
    InferenceJarPath("inferencejars/org.jetbrains.kotlinx.kotlinx-coroutines-android-1.3.9.jar", isResource = true),
    InferenceJarPath("inferencejars/rxjava-2.1.0.jar", isResource = true)
  )
}
