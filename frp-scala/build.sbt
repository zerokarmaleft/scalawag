name := "scalawag"

version := "0.0.1"

scalaVersion := "2.10.3"

scalacOptions ++= Seq(
  "-deprecation",
  "-encoding", "UTF-8",
  "-feature",
  "-Xlint",
  "-Ywarn-all"
)

libraryDependencies ++= Seq(
  "com.netflix.rxjava" % "rxjava-scala" % "0.16.1",
  "com.netflix.rxjava" % "rxjava-swing" % "0.16.1",
  "org.scalaz" %% "scalaz-core" % "7.0.5",
  "com.googlecode.lanterna" % "lanterna" % "2.1.7"
)
