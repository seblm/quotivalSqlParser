import annotation.target
import sbt.{TestFrameworks, Tests, Build}

class BuildMyProject extends Build {

    testOptions in Test <+= (target in Test) map {
        t => Tests.Argument(TestFrameworks.ScalaTest, "junitxml(directory=\"%s\")" format (t / "test-reports"))
    }

}
