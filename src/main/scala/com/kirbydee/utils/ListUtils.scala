package com.kirbydee.utils

import scala.annotation.tailrec
import scala.collection.IterableLike
import scala.collection.generic.CanBuildFrom
import com.kirbydee.utils.Ternary.Implicits._
import scala.language.{implicitConversions, postfixOps}

object ListUtils {

    /**
      * Implicit Classes, Conversions etc.
      */
    trait Implicits {

        /**
          * Any implicit class Helper
          *
          * @param t Anything
          */
        implicit class AnyHelper[T](t: T) {

            /**
              * Creates a sequence out of the element
              *
              * @return Seq(t)
              */
            @inline def seq: Seq[T] =
                Seq(t)

            /**
              * Creates a list out of the element
              *
              * @return List(t)
              */
            @inline def list: List[T] =
                List(t)

            /**
              * Creates a set out of the element
              *
              * @return Set(t)
              */
            @inline def set: Set[T] =
                Set(t)
        }

        /**
          * Iterable implicit class Helper
          *
          * @param xs The iterable
          */
        implicit class IterableHelper[A, Repr](xs: IterableLike[A, Repr]) {

            /**
              * Creates a copy of the iterable with only distinct elements in it by the function given.
              *
              * @param f The "by" function
              * @return distinct collection
              */
            @inline def distinctBy[B, That](f: A => B)(implicit cbf: CanBuildFrom[Repr, A, That]) = {
                val builder = cbf(xs.repr)
                val i = xs.iterator
                var set = Set[B]()
                while (i.hasNext) {
                    val o = i.next
                    val b = f(o)
                    if (!set(b)) {
                        set += b
                        builder += o
                    }
                }
                builder.result
            }
        }

        /**
          * List implicit class Helper
          *
          * @param l The list
          */
        implicit class ListHelper[T](l: List[T]) extends ContainerHelper[T, List](l) {

            /**
              * Gets the element in the List.
              *
              * @return List.get
              */
            override protected def get: T =
                l.head

            /**
              * Checks, if List is empty or not.
              *
              * @return List.nonEmpty
              */
            @inline override def ? : Boolean =
                l.nonEmpty

            /**
              * Runs the contain function on the List
              *
              * @param e The element
              * @return List.contain
              */
            @inline def ?(e: T): Boolean =
                l contains e

            /**
              * Runs the exist function on the List
              *
              * @param f The exists function
              * @return List.exist
              */
            @inline def ?(f: T => Boolean): Boolean =
                l exists f

            /**
              * Maps over the List
              *
              * @param f The mapping function
              * @return List.map
              */
            @inline override def >[B](f: (T) => B): List[B] =
                l map f

            /**
              * FlatMaps over the List
              *
              * @param f The mapping function
              * @return List.flatMap
              */
            @inline override def >>[B](f: (T) => List[B]): List[B] =
                l flatMap f

            /**
              * Filters on given function
              *
              * @return List.filter
              */
            @inline override def <~(f: (T) => Boolean): List[T] =
                l filter f

            /**
              * Excludes the list given from THIS list.
              *
              * @param l2 The list to exclude
              * @return THIS list with l2 excluded
              */
            @inline def exclude(l2: List[T]): List[T] =
                l.filterNot(l2.toSet)
        }

        /**
          * List Option implicit class Helper
          *
          * @param l The list with options inside
          */
        implicit class ListOptionHelper[T](l: List[Option[T]]) {

            /**
              * Filters the list on just the Some values (the existing ones).
              *
              * @return List.some
              */
            @inline def somes: List[T] = l collect {
                case Some(t) => t
            }

            /**
              * Gets the first existing Some value in the list.
              *
              * @return List.some.headOption
              */
            @inline def headSome: Option[T] =
                somes.headOption
        }

        /**
          * List List implicit class Helper
          *
          * @param list The list with lists inside
          */
        implicit class ListListHelper[T](list: List[List[T]]) {

            /**
              * Returns the intersection of all the lists inside of the list
              *
              * @param f Function to check for intersection (equality check)
              * @return intersection with the lists inside of the list
              */
            @inline def inter[A](f: T => A): List[T] = {
                @tailrec
                def find(elem: T, searchList: List[List[T]], found: Boolean): Boolean = (found, searchList) match {
                    case (false, _) | (_, Nil) => found
                    case (true, l :: ls)       => find(elem, ls, l.exists(c => f(c) == f(elem)))
                }

                @tailrec
                def go(list1: List[T], lists: List[List[T]], intersection: List[T]): List[T] = list1 match {
                    case Nil       => intersection
                    case l1 :: l1s => go(l1s, lists, intersection ++ (find(l1, lists, found = true) ? List(l1) | Nil))
                }

                // check for enough list
                list match {
                    case Nil      => Nil
                    case l :: Nil => l
                    case l :: ls  => go(l ,ls, Nil)
                }
            }
        }
    }
    object Implicits extends Implicits
}