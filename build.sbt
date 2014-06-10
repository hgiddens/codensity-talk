name := "codensity-talk"

scalaVersion := "2.10.4"

libraryDependencies ++= Seq(
  "com.github.axel22" %% "scalameter" % "0.4",
  "org.scalaz" %% "scalaz-core" % "7.1.0-M7"
)

testFrameworks += new TestFramework("org.scalameter.ScalaMeterFramework")

parallelExecution in Test := false
