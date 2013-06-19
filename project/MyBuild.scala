import sbt._
import sbt.Keys._

object MyBuild extends Build {
    lazy val root = Project(id = "quotivalSqlParser", base = file("."),
        settings = Project.defaultSettings ++ Seq(

        name := "quotivalSqlParser",

        scalaVersion := "2.10.2",

        libraryDependencies := Seq(
            "org.scalaz" %% "scalaz-core" % "7.0.0",
            "org.scalatest" % "scalatest_2.10" % "2.0.M5b" % "test"
        ),

        testOptions in Test <+= (target in Test) map {
            t => Tests.Argument(TestFrameworks.ScalaTest, "junitxml(directory=\"%s\")" format (t / "test-reports"), "-o")
        }
    ))

}
