package it.unibo.pps.tasks.adts

import org.junit.*
import org.junit.Assert.*
import Ex3Stacks.StackImpl
import it.unibo.pps.u03.Sequences.Sequence
import it.unibo.pps.u03.Optionals.Optional

class Stacktest:
  
  val stack: Ex3Stacks.StackImpl.type = StackImpl

  import stack.*
  
  @Test def testEmptyStackHasNoElements(): Unit =
    assertEquals(Sequence.Nil(), empty[Int].asSequence())
  
  @Test def testPushAddsElementToStack(): Unit =
    assertEquals(Sequence.Cons(10, Sequence.Nil()), empty[Int].push(10).asSequence())
  
  @Test def testPopOnEmptyStackReturnsEmpty(): Unit =
    assertEquals(Optional.Empty(), empty[Int].pop())
  
  @Test def testPopOnStackWithOneElementReturnsElementAndEmptyStack(): Unit =
    assertEquals(Optional.Just((10, empty[Int])), empty[Int].push(10).pop())
  
  @Test def testPushMultipleElementsAndVerifyOrder(): Unit =
    val stack = empty[Int].push(10).push(20).push(30)
    assertEquals(Sequence.Cons(30, Sequence.Cons(20, Sequence.Cons(10, Sequence.Nil()))), stack.asSequence())
  
  @Test def testPopMultipleElementsMaintainsOrder(): Unit =
    val stack = empty[Int].push(10).push(20)
    val popResult = stack.pop()
    assertEquals(Optional.Just((20, empty[Int].push(10))), popResult)