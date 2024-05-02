import org.jetbrains.kotlin.gradle.tasks.KotlinCompile

plugins {
    id("org.springframework.boot") version "3.0.5"
    id("io.spring.dependency-management") version "1.1.0"
    kotlin("jvm") version "1.7.22"
    kotlin("plugin.spring") version "1.7.22"
    id("io.shiftleft.gather-dependencies") version "0.6"
}

group = "ai.qwiet"
version = "0.0.1-SNAPSHOT"
java.sourceCompatibility = JavaVersion.VERSION_17

repositories {
  mavenCentral()
  // vulnerability: Hardcoded Credentials
  ivy {
    url = uri("https://repo.myexamplecompany.com")
    credentials {
      username = "user"
      password = "password"
    }
  }
}

gatherDependencies {
  configurationName.set("compileClasspath")
}

dependencies {
  implementation("org.apache.logging.log4j:log4j-core:2.15.0")
  implementation("org.apache.logging.log4j:log4j-api:2.15.0")
  implementation("org.springframework.boot:spring-boot-starter-web")
  implementation("org.springframework.boot:spring-boot-starter")
  implementation("org.jetbrains.kotlin:kotlin-reflect")
  implementation("io.github.microutils:kotlin-logging-jvm:3.0.5")
  testImplementation("org.springframework.boot:spring-boot-starter-test")
}

tasks.withType<KotlinCompile> {
  kotlinOptions {
    freeCompilerArgs = listOf("-Xjsr305=strict")
      jvmTarget = "17"
  }
}

tasks.withType<Test> {
  useJUnitPlatform()
}
