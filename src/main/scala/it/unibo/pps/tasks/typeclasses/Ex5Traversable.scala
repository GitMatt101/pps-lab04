package it.unibo.pps.tasks.typeclasses

import it.unibo.pps.u03.Sequences.Sequence
import Sequence.*
import it.unibo.pps.u03.Optionals.Optional
import it.unibo.pps.u03.Optionals.Optional.Just

import scala.annotation.tailrec

/*  Exercise 5: 
 *  - Generalise by ad-hoc polymorphism logAll, such that:
 *  -- it can be called on Sequences but also on Optional, or others... 
 *  -- it does not necessarily call log, but any function with analogous type
 *  - Hint: introduce a type class Traversable[T[_]]], capturing the ability of calling a
 *    "consumer function" on all elements (with type A) of a data structure T[A]
 *    Note Traversable is a 2-kinded trait (similar to Filterable, or Monad)
 *  - Write givens for Traversable[Optional] and Traversable[Sequence]
 *  - Show you can use the generalisation of logAll to:
 *  -- log all elements of an Optional, or of a Traversable
 *  -- println(_) all elements of an Optional, or of a Traversable
 */

object Ex5Traversable:

  def log[A](a: A): Unit = println("The next element is: "+a)

  def logAll[T[_], A](container: T[A])(consumer: A => Unit)(using t: Traversable[T]): Unit =
    t.foreach(container)(consumer)

  trait Traversable[T[_]]:
    def foreach[A](container: T[A])(consumer: A => Unit): Unit

  given Traversable[Sequence] with
    override def foreach[A](container: Sequence[A])(consumer: A => Unit): Unit = container match
      case Cons(h, t) => consumer(h); foreach(t)(consumer)
      case _ => ()

  given Traversable[Optional] with
    override def foreach[A](container: Optional[A])(consumer: A => Unit): Unit = container match
      case Just(elem) => consumer(elem)
      case _ => ()


  @main def tryTraversables(): Unit =
    import it.unibo.pps.u03.Sequences.Sequence.*
    import it.unibo.pps.u03.Optionals.Optional.*

    val seq = Cons(1, Cons(2, Cons(3, Cons(4, Cons(5, Nil())))))
    val opt = Just("hello")
    val emptyOpt = Empty()

    println("--- Testing Sequence (1 to 5) ---")
    logAll(seq)(log)

    println("\n--- Testing Optional ('hello') ---")
    logAll(opt)(log)

    println("\n--- Testing Empty Optional ---")
    logAll(emptyOpt)(log)