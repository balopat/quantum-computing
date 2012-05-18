package com.balopat.qc;

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import ComplexNumberRectangular._




class ComplexNumberSpec extends FlatSpec with ShouldMatchers {

  "A Complex Number" should
    "add to another Complex Number by components" in {
    val a = (3 i) + 1
    val b = (2 i) + 2
    a + b should be ( (5 i) + 3)
  }

  it should "substract another Complex Number by components" in {
    val a = (2 i) + 1
    val b = (3 i) + 2
    a - b should be ( (-1 i) - 1)
  }

  "i" should
    "give -1 multiplied by i" in {
    (1 i) * (1 i)  should be ( (0 i) - 1)
  }

  "ai + b * ci+ d " should
    "be equal to (bc + da)i + bd-ac" in {
    val a = 2
    val b = 3
    val c = 4
    val d = 5
    ((a i) + b)*((c i) + d)  should be ( ((b*c+d*a) i) + b*d - a*c)
  }

}