package com.kirbydee.config

object Environment {

    def apply(env: String): Environment = env match {
        case Production.name  => Production
        case Development.name => Development
        case _                => Localhost
    }
}
abstract class Environment(val name: String) {

    def get[A](production: => A)(development: => A)(local: => A): A = this match {
        case Production  => production
        case Development => development
        case Localhost   => local
    }

    override def toString: String =
        this.name
}
case object Production  extends Environment("production")
case object Development extends Environment("development")
case object Localhost   extends Environment("localhost")