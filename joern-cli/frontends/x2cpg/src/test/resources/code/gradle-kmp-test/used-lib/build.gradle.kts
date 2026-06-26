plugins {
    kotlin("multiplatform") version "2.1.21"
}

kotlin {
    jvm()
    val host = System.getProperty("os.name").lowercase()
    when {
        host.contains("mac")   -> macosArm64("native")
        host.contains("linux") -> linuxX64("native")
        host.contains("win")   -> mingwX64("native")
        else                   -> linuxX64("native")
    }
}
