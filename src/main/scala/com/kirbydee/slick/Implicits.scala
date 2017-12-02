package com.kirbydee.slick

import com.github.tototoshi.slick.GenericJodaSupport
import slick.driver.{MySQLDriver, PostgresDriver}

trait Implicits extends

        // run Implicits
        Run.Implicits

        // slick aliases
        with slick.lifted.Aliases
object Implicits extends Implicits

// MySQL driver implicits
object ImplicitsMySQL extends

        // Joda support
        GenericJodaSupport(MySQLDriver)

        // common Implicits
        with Implicits

        // query Implicits
        with QueryMySQL.Implicits

        // with MysqlDriver
        with MySQLDriver

// Postgres driver implicits
object ImplicitsPostgres extends

        // Joda support
        GenericJodaSupport(PostgresDriver)

        // common Implicits
        with Implicits

        // query Implicits
        with QueryPostgres.Implicits

        // with PostgresDriver
        with PostgresDriver

