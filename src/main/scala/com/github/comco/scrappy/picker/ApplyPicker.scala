package com.github.comco.scrappy.picker

import scala.reflect.runtime.universe._

import com.github.comco.scrappy._

/**
 * Picker for applying a raw scala function to an element
 */
case class ApplyPicker[A: TypeTag, R: TypeTag](f: A => R)(
  implicit val sourceType: Type.Primitive[A], val targetType: Type.Primitive[R])
    extends Picker[Shape.Primitive[A], Shape.Primitive[R]] {

  def pickData(source: Data.Primitive[A]): Data.Primitive[R] = {
    Data.Primitive(f(source.value))
  }

  def pickOriginatedData(source: OriginatedData.Primitive[A]): OriginatedData.Primitive[R] = {
    OriginatedData(pickData(source.data), source.origin.computed)
  }
}