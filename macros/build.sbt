name := "macros"
version := "1.0"
scalaVersion := "2.11.8"
organization := "com.logograb"

resolvers ++= Seq(
    "Typesafe repository" at "http://repo.typesafe.com/typesafe/releases/",
    "productfoundry at bintray" at "http://dl.bintray.com/productfoundry/maven",
    "Kaliber Internal Repository" at "https://jars.kaliber.io/artifactory/libs-release-local"
)

libraryDependencies ++= List(
    // macro
    "org.scala-lang"      % "scala-reflect"    % "2.11.8",
    "com.productfoundry" %% "play-json-zipper" % "1.3",

    // reflection
    "org.reflections" % "reflections"   % "0.9.10",
    "org.scala-lang"  % "scala-reflect" % "2.11.5"
)