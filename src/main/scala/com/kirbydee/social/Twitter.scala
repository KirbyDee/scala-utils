package com.kirbydee.social

import javax.inject.Inject

import com.kirbydee.config.{Configuration, TwitterConfig}
import twitter4j.TwitterFactory
import twitter4j.conf.ConfigurationBuilder

class Twitter @Inject()(config: Configuration) {

    // twitter configuration
    private val twitter: TwitterConfig = config.twitter

    // build configuration
    private val configurationBuilder: ConfigurationBuilder = new ConfigurationBuilder()
            .setOAuthConsumerKey(twitter.consumerKey)
            .setOAuthConsumerSecret(twitter.consumerSecret)
            .setOAuthAccessToken(twitter.accessToken)
            .setOAuthAccessTokenSecret(twitter.accessTokenSecret)
            .setTweetModeExtended(true)

    // build factory
    private val factory: TwitterFactory = new TwitterFactory(configurationBuilder.build())

    // get Instance
    def getInstance: twitter4j.Twitter =
        factory.getInstance
}