plugins {
    java
    scala
    id("com.github.johnrengelman.shadow") version "4.0.3"
    application
}

repositories {
    mavenCentral()
}

dependencies {
    implementation("org.scala-lang:scala-library:2.12.2")
    implementation("it.unibo.scafi:scafi-core_2.12:0.3.3")
}

application {
    mainClass.set("xc.examples.GradientMain")
}