package com.balopat.qc

import scala.math._

case class ComplexNumberCartesian(im: Double, r: Double) {

  def i() = ComplexNumberCartesian(r, im)

  def +(b: ComplexNumberCartesian) = ComplexNumberCartesian(im + b.im, r + b.r)

  def +(b: Double) = ComplexNumberCartesian(im, r + b)

  def -(b: ComplexNumberCartesian) = ComplexNumberCartesian(im - b.im, r - b.r)

  def -(b: Double) = ComplexNumberCartesian(im, r - b)

  def *(b: ComplexNumberCartesian) = ComplexNumberCartesian(r * b.im + b.r * im, r * b.r - im * b.im)

  def /(b: Double) = ComplexNumberCartesian(im / b, r / b)

  def /(b: ComplexNumberCartesian): ComplexNumberCartesian = {
    ComplexNumberCartesian((b.r * im - r * b.im), (r * b.r + im * b.im)) / b.modSquare
  }

  def modSquare() = im * im + r * r

  def modulus() = sqrt(modSquare)


}

object ComplexNumberCartesian {

  implicit def any2CN(x: Double): ComplexNumberCartesian = ComplexNumberCartesian(0, x)

  implicit def any2CN(x: Int): ComplexNumberCartesian = ComplexNumberCartesian(0, x)

  def ^(a: ComplexNumberCartesian) = ComplexNumberCartesian(-a.im, a.r)
}