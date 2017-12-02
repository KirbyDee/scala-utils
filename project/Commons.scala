import sbt.Keys._
import sbt._

object Commons {
    val settings: Seq[Def.Setting[_]] = Seq(
        scalaVersion := "2.11.8",
        organization := "com.logograb",
        version := "1.0",
        resolvers ++= Seq(
            "Typesafe repository" at "http://repo.typesafe.com/typesafe/releases/",
            "productfoundry at bintray" at "http://dl.bintray.com/productfoundry/maven",
            "Kaliber Internal Repository" at "https://jars.kaliber.io/artifactory/libs-release-local"
        )
    )
}