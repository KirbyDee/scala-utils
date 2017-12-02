package com.kirbydee.utils

import scala.language.{higherKinds, implicitConversions, postfixOps}

abstract class ContainerHelper[A, C[+_]](c: C[A]) {

    /**
      * Gets the element in the Container.
      *
      * @return Container.get
      */
    protected def get: A

    /**
      * Checks, if Container is defined or not.
      *
      * @return Container.isDefined
      */
    @inline def ? : Boolean

    /**
      * Checks, if Container is defined or not.
      *
      * @return Option.isEmpty
      */
    @inline def !? : Boolean =
        ! ?

    /**
      * Get Or Else.
      *
      * @param f The GetOrElse function
      * @return Container.getOrElse
      */
    @inline def |[AA >: A](f: => AA): AA = ? match {
        case true  => get
        case false => f
    }

    /**
      * Or Else.
      *
      * @param f The OrElse function
      * @return Container.orElse
      */
    @inline def ||[AA >: A](f: => C[AA]): C[AA] = ? match {
        case true  => c
        case false => f
    }

    /**
      * Maps over the Container
      *
      * @param f The mapping function
      * @return Container.map
      */
    @inline def >[B](f: A => B): C[B]

    /**
      * FlatMaps over the Container
      *
      * @param f The mapping function
      * @return Container.flatMap
      */
    @inline def >>[B](f: A => C[B]): C[B]

    /**
      * Maps over the Container
      *
      * @param f The independent value function to map to
      * @return Container.map
      */
    @inline def >>>[B](f: => B): C[B] =
        > (_ => f)

    /**
      * Filters on given function
      *
      * @return Container.filter
      */
    @inline def <~(f: A => Boolean): C[A]
}