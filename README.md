# XC: Scala DSL implementation

## XC/Scala: an XC implementation as an internal Scala domain-specific language (DSL)

- **Where?** The source code of the implementation can be found under `src/main/scala`, package `xc`
- **What?** The DSL implementation includes the components described in the paper, namely:
    - `xc.NValues#NValue`: data structure modelling a neighbouring value
    - `xc.XCLang` and `xc.XCLangImpl`: interface with the XC constructs and corresponding implementation class
    - `xc.XCLib`: provides reusable functions (building blocks) upon `XCLang`; examples include `collect`, `broadcast`, `distanceTo` (see examples in the paper)
    - `xc.XCProgram`: class to be specialised and implemented (`main` method) in order to define an XC program
    - `xc.examples.Gradient`: shows a concrete XC program computing a "gradient" (each node of the system eventually computes the minimum distance from the closest "source" node)
- **How??**
    - Run `./gradlew run` to execute an XC system of interacting devices computing a program
    - Take a look at `xc.examples.GradientMain` to get a sense of the execution model
