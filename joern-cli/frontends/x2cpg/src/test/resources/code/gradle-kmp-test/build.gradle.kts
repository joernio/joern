plugins {
    kotlin("multiplatform") version "2.1.21"
}

// NOTE: Kotlin Multiplatform's JVM target DOES apply `java-base`, so branch
// (a) of the init script would surface `jvmMain` — but ONLY `jvmMain`.
// `commonMain` and `nativeMain` are invisible to `sourceSets.main.allSource`.
// Branch (c) walks `kotlin.sourceSets` to recover the full set.

kotlin {
    jvm()
    // Pick whichever native target matches the host so this resolves on any dev box.
    val host = System.getProperty("os.name").lowercase()
    when {
        host.contains("mac")   -> macosArm64("native")
        host.contains("linux") -> linuxX64("native")
        host.contains("win")   -> mingwX64("native")
        else                   -> linuxX64("native")
    }

    sourceSets {
        val commonMain by getting {
            dependencies {
                // Root project depends on :used-lib but NOT on :unused-lib.
                // The dependency fetcher should record `:used-lib` in the
                // root's sourceDependencies and emit a separate depInfo
                // file for `:unused-lib` that nothing references back.
                implementation(project(":used-lib"))
            }
        }
        val jvmMain    by getting
        val nativeMain by getting
    }
}
