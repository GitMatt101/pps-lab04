package it.unibo.pps.tasks.adts

/*  Exercise 1: 
 *  Complete the implementation of ComplexADT trait below, so that it passes
 *  the test in ComplexTest.
 */

object Ex1ComplexNumbers:

  trait ComplexADT:
    type Complex
    def complex(re: Double, im: Double): Complex
    extension (complex: Complex)
      def re(): Double
      def im(): Double
      def sum(other: Complex): Complex
      def subtract(other: Complex): Complex
      def asString(): String

  object BasicComplexADT extends ComplexADT:

    // Change assignment below: should probably define a case class and use it?
    type Complex = BasicComplex
    def complex(re: Double, im: Double): Complex = BasicComplex(re, im)

    extension (complex: Complex)
      def re(): Double = complex match
        case BasicComplex(re, _) => re
        case _ => 0

      def im(): Double =  complex match
        case BasicComplex(_, im) => im
        case _ => 0

      def sum(other: Complex): Complex =
        BasicComplex(complex.re() + other.re(), complex.im() + other.im())

      def subtract(other: Complex): Complex =
        BasicComplex(complex.re() - other.re(), complex.im() - other.im())

      def asString(): String =
        val re = complex.re()
        val im = complex.im()
        (re, im) match
          case (0, 0) => "0.0"
          case (_, 0) => "" + re
          case (0, _) => im + "i"
          case (_, x) if x > 0 => re + " + " + im + "i"
          case _ => re + " - " + im * -1 + "i"
