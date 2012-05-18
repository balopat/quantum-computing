package com.balopat.qc


case class ComplexNumberRectangular(im: Int, r: Int) {

  def i() = {
    this
  };

  def +(b: ComplexNumberRectangular) = ComplexNumberRectangular(im + b.im, r + b.r)

  def +(b: Int) = ComplexNumberRectangular(im, r + b)

  def -(b: ComplexNumberRectangular) = ComplexNumberRectangular(im - b.im, r - b.r)

  def -(b: Int) = ComplexNumberRectangular(im, r - b)

  def *(b: ComplexNumberRectangular) = {
      ComplexNumberRectangular(r*b.im+b.r*im, r*b.r-im*b.im)
  }
}

object ComplexNumberRectangular {
  implicit def any2CN(x: Int): ComplexNumberRectangular = ComplexNumberRectangular(x, 0)
}