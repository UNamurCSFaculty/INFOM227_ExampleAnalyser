plugins {
    application
    scala
    antlr
    id("com.gradleup.shadow") version "9.6.1"
}

application {
    mainClass = "be.unamur.info.infom227.main"
}

java {
    toolchain {
        languageVersion = JavaLanguageVersion.of(23)
    }
}

group = "be.unamur.info.infom227"
version = "2.0.0"

repositories {
    mavenCentral()
}

dependencies {
    implementation("org.scala-lang:scala3-library_3:3.8.4")
    antlr("org.antlr:antlr4:4.13.2")

    testImplementation("org.scalatest:scalatest_3:3.2.20")
    testRuntimeOnly("org.junit.platform:junit-platform-engine:1.14.4")
    testRuntimeOnly("org.junit.platform:junit-platform-launcher:1.14.4")
    testRuntimeOnly("org.scalatestplus:junit-5-14_3:3.2.20.0")
}

tasks.test {
    useJUnitPlatform {
        includeEngines("scalatest")
        testLogging {
            events("passed", "skipped", "failed", "standard_error")
        }
    }
}

tasks.generateGrammarSource {
    arguments = arguments + listOf("-package", "be.unamur.info.infom227.cst", "-visitor", "-no-listener")
}
