plugins {
    application
    scala
    antlr
    id("com.gradleup.shadow") version "8.3.3"
}

application {
    mainClass = "be.unamur.info.infom227.main"
}

java {
    toolchain {
        languageVersion = JavaLanguageVersion.of(17)
    }
}

group = "be.unamur.info.infom227"
version = "1.0.0"

repositories {
    mavenCentral()
}

dependencies {
    implementation("org.scala-lang:scala3-library_3:3.5.2")
    antlr("org.antlr:antlr4:4.13.2")

    testImplementation("org.scalatest:scalatest_3:3.2.19")
    testRuntimeOnly("org.junit.platform:junit-platform-engine:1.10.2")
    testRuntimeOnly("org.junit.platform:junit-platform-launcher:1.10.2")
    testRuntimeOnly("org.scalatestplus:junit-5-10_3:3.2.19.0")
}

tasks.test {
    useJUnitPlatform {
        includeEngines("scalatest")
        testLogging {
            events("passed", "skipped", "failed")
        }
    }
}

tasks.generateGrammarSource {
    arguments = arguments + listOf("-package", "be.unamur.info.infom227.cst", "-visitor", "-no-listener")
}
