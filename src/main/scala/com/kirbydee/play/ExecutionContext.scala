package com.kirbydee.play

import play.api.libs.concurrent.Execution

object ExecutionContext {

    /**
      * Implicit Classes, Conversions etc.
      */
    trait Implicits {

        /*
         * val / types assignments for easier access
         */
        implicit def defaultContext = Execution.defaultContext
    }
    object Implicits extends Implicits
}
