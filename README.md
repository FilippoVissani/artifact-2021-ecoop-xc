# XC: Scala DSL implementation

XC/Scala is a Scala-internal domain-specific language (DSL) implementation of the XC research language for distributed adaptive computing.

## Overview

- **Where?** The source code of the implementation can be found under `src/main/scala`, package `xc`
- **What?** The DSL implementation includes the components described in the paper, namely:
    - `xc.NValues#NValue`: data structure modelling a neighbouring value
    - `xc.XCLang` and `xc.XCLangImpl`: interface with the XC constructs and corresponding implementation class
    - `xc.XCLib`: provides reusable functions (building blocks) upon `XCLang`; examples include `collect`, `broadcast`, `distanceTo` (see examples in the paper)
    - `xc.XCProgram`: class to be specialised and implemented (`main` method) in order to define an XC program
    - `xc.examples.Gradient`: shows a concrete XC program computing a "gradient" (each node of the system eventually computes the minimum distance from the closest "source" node).
    This is especially illustrative since it shows a simple implementation of a "complete system" where communication is simulated by writing on shared state.
    However, please notice that the synchronous execution is a simplification: the devices of an XC system may run concurrently and communications can be completely asynchronous.
- **How??**
    - Run `./gradlew run` (Linux) or `gradlew.bat run` (Windows) to execute an XC system of interacting devices computing a program
        - Prerequisites: Java 11 (e.g., using Jabba: `jabba use adopt@1.11.28-0`)
    - Take a look at `xc.examples.GradientMain` to get a sense of the execution model

## License

See the `LICENSE` file.
