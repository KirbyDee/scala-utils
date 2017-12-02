import sbt._

object Dependencies {

    // Modules
    val slick:      Seq[ModuleID] = Seq(
        "com.typesafe.play"    %% "play-slick"        % "2.0.0",
        "com.github.tototoshi" %% "slick-joda-mapper" % "2.2.0",
        "joda-time"            %  "joda-time"         % "2.7",
        "org.joda"             %  "joda-convert"      % "1.7"
    )
    val reflection: Seq[ModuleID] = Seq(
        "org.reflections" % "reflections"   % "0.9.10",
        "org.scala-lang"  % "scala-reflect" % "2.11.5"
    )
    val play:       Seq[ModuleID] = Seq(
        "com.typesafe.play"  %  "play-logback_2.11"    % "2.5.8",
        "com.typesafe.play"  %% "play-ws"              % "2.5.8",
        "com.typesafe.play"  %% "play-mailer"          % "6.0.1",
        "com.typesafe.play"  %% "play-mailer-guice"    % "6.0.1"
    )
    val twitter4j: Seq[ModuleID] = Seq(
        "org.twitter4j" % "twitter4j-core"   % "4.0.6",
        "org.twitter4j" % "twitter4j-stream" % "4.0.6"
    )
    val jsonZipper: ModuleID = "com.productfoundry" %% "play-json-zipper"     % "1.3"
    val mysql:      ModuleID = "mysql"              %  "mysql-connector-java" % "5.1.37"
    val jdbc:       ModuleID = "com.heroku.sdk"     %  "heroku-jdbc"          % "0.1.1"
    val apache:     ModuleID = "org.apache.commons" %  "commons-io"           % "1.3.2"
    val codec:      ModuleID = "commons-codec"      %  "commons-codec"        % "1.9"
    val scalaz:     ModuleID = "org.scalaz"         %% "scalaz-core"          % "7.2.7"
    val aws:        ModuleID = "com.github.seratch" %% "awscala"              % "0.5.+"
    val javax:      ModuleID = "javax.inject"       %  "javax.inject"         % "1"

    // Common Modules
    val common: Seq[ModuleID] = Seq(
        jsonZipper,
        mysql,
        jdbc,
        apache,
        codec,
        scalaz,
        aws,
        javax
    ) ++ slick ++ reflection ++ play ++ twitter4j
}