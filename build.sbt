name := "pbdirect"

scalaVersion := "2.12.2"

libraryDependencies ++= Seq(
  "com.chuusai"         %% "shapeless"     % "2.3.2",
  "org.typelevel"       %% "cats"          % "0.9.0",
  "com.google.protobuf" %  "protobuf-java" % "3.2.0",
  "org.scalatest"       %% "scalatest"     % "3.0.1" % Test
)