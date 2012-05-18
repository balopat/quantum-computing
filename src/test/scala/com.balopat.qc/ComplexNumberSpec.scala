package com.balopat.qc

;

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import ComplexNumberCartesian._

class ComplexNumberSpec extends FlatSpec with ShouldMatchers {

  "A Complex Number" should
    "add to another Complex Number by components" in {
    val a = (3 i) + 1
    val b = (2 i) + 2
    a + b should be((5 i) + 3)
  }

  it should "substract another Complex Number by components" in {
    val a = (2 i) + 1
    val b = (3 i) + 2
    a - b should be((-1 i) - 1)
  }

  it should "support addition of Ints from the left side too" in {
    1 + (2 i) should be((2 i) + 1)
  }

  it should "support any kind of addition" in {
    1 + (3 i) + 4 + (6 i) - 3 should be((9 i) + 2)
  }

  it should "support any numeric" in {
    (3.01 i) + 66.99 / 3 should be((3.01 i) + 22.33)
  }

  "i" should
    "give -1 multiplied by i" in {
    (1 i) * (1 i) should be((0 i) - 1)
  }

  "ai + b * ci+ d " should
    "be equal to (bc + da)i + bd-ac" in {
    val a = 2
    val b = 3
    val c = 4
    val d = 5
    ((a i) + b) * ((c i) + d) should be(((b * c + d * a) i) + b * d - a * c)
  }

  "The modulus for ai + b" should
    "be sqrt(a*a + b*b)" in {
    ((2 i) + 4).modulus should be(scala.math.sqrt(2 * 2 + 4 * 4))
  }

  "The conjugate for ai + b" should
    "be -ai + b" in {
    ^((2 i) + 4) should be((-2 i) + 4)
  }

  "Given a * b = c all in C, when c/b calculated, it " should
    "result in a" in {
    val b = (2 i) + 1
    val c = (3 i) + 2
    val a = c / b

    a * b should be(c)
  }


}