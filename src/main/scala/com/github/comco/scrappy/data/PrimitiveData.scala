package com.github.comco.scrappy.data

import scala.language.implicitConversions

import com.github.comco.scrappy.PrimitiveType
import com.github.comco.scrappy.data.simple.SimplePrimitiveData

abstract class PrimitiveData[T] extends Data.Base {
  def datatype: PrimitiveType[T]

  /**
   * The raw value of this primitive data.
   */
  def value: T
}

object PrimitiveData {
  implicit def apply[T](value: T)(
    implicit datatype: PrimitiveType[T]): PrimitiveData[T] =
    SimplePrimitiveData(value)
}