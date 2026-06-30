plugins {
    kotlin("jvm") version "1.9.23"
}

group = "com.example"
version = "1.0.0"

repositories {
    mavenCentral()
}

tasks.withType<org.jetbrains.kotlin.gradle.tasks.KotlinCompile> {
    kotlinOptions {
        jvmTarget = "17"
    }
}

// Wire the kotlin source root from a sibling directory that lives outside this
// Gradle build root. Used as a regression fixture for the
// transitiveDependenciesForProjectsInDir empty-filter fallback.
sourceSets {
    main {
        kotlin {
            setSrcDirs(listOf(rootProject.file("../shared/src/main/kotlin")))
        }
    }
}
