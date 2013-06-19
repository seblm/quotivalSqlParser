name := "quotivalSqlParser"

scalaVersion := "2.10.2"

libraryDependencies += "org.scalatest" % "scalatest_2.10" % "2.0.M5b" % "test"

libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.0.0"

testOptions in Test <+= (target in Test) map {
  t => Tests.Argument(TestFrameworks.ScalaTest, "junitxml(directory=\"%s\")" format (t / "test-reports"))
}