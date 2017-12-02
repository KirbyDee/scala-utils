package com.kirbydee.config

import com.typesafe.config.ConfigFactory
import com.kirbydee.utils.Implicits._

import scala.language.postfixOps

object Configuration extends Configuration { self =>

    // self referencing
    val config = self
}

class Configuration {
    // Configuration File
    private val config = ConfigFactory.load()

    // System properties
    private val props = sys.props

    // aws configurations
    val aws: AWSConfig = config hasPath "logograb" match {
        case false =>
            defaultAWSConfig

        case true  =>
            // root
            val logograb = config getConfig "logograb"
            logograb hasPath "aws" match {
                case false =>
                    defaultAWSConfig

                case true  =>
                    // aws configurations
                    val aws       = logograb getConfig "aws"
                    val awsKey    = (props get "awsKey")    | (aws getString "key")
                    val awsSecret = (props get "awsSecret") | (aws getString "secret")
                    val awsRegion = (props get "awsRegion") | (aws getString "region")

                    // create config
                    AWSConfig(awsKey, awsSecret, awsRegion)
            }

    }

    // twitter configurations
    val twitter: TwitterConfig = config hasPath "logograb" match {
        case false =>
            defaultTwitterConfig

        case true  =>
            // root
            val logograb = config getConfig "logograb"
            logograb hasPath "twitter" match {
                case false =>
                    defaultTwitterConfig

                case true  =>
                    // twitter configurations
                    val twitter            = logograb getConfig "twitter"
                    val twitterConsumer    = twitter getConfig "consumer"
                    val consumerKey        = (props get "twitterConsumerKey")    | (twitterConsumer getString "key")
                    val consumerSecret     = (props get "twitterConsumerSecret") | (twitterConsumer getString "secret")
                    val twitterAccess      = twitter getConfig "access"
                    val accessToken        = (props get "twitterAccessToken")  | (twitterAccess getString "key")
                    val accessTokenSecret  = (props get "twitterAccessSecret") | (twitterAccess getString "secret")

                    // create config
                    TwitterConfig(consumerKey, consumerSecret, accessToken, accessTokenSecret)
            }

    }
    private def defaultAWSConfig: AWSConfig =
        AWSConfig("", "", "")

    private def defaultTwitterConfig: TwitterConfig =
        TwitterConfig("", "", "", "")
}

case class AWSConfig(key: String, secret: String, region: String)
case class TwitterConfig(consumerKey: String, consumerSecret: String, accessToken: String, accessTokenSecret: String)