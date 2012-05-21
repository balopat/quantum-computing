package com.balopat.qc;

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import ComplexNumberCartesian._
import scala.math._


class ComplexNumberPolarSpec extends FlatSpec with ShouldMatchers {

  "A Complex Number in Polar representation (ρ, θ)" should
    "convert to a Cartesian representation a = ρ cos(θ) b = sin(θ) " in {
    val θ: Double = 3 * Pi
    val ρ: Double = 2
    ComplexNumberPolar(ρ, θ).asCartesian should be((ρ * cos(θ) i) + ρ * sin(θ))
  }

}