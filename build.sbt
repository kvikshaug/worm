name := "Worm"

version := "0.5"

scalaVersion := "2.9.1"

libraryDependencies ++= Seq(
  "org.scalatest" % "scalatest_2.9.0" % "1.6.1" % "test",
  "junit" % "junit" % "4.9" % "test",
  "com.novocode" % "junit-interface" % "0.7" % "test",
  "org.xerial" % "sqlite-jdbc" % "3.7.2"
)
