package com.github.comco.scrappy.picker

import scala.reflect.runtime.universe._

import com.github.comco.scrappy.data.Data
import com.github.comco.scrappy.originated_data.OriginatedData
import com.github.comco.scrappy.Type
import com.github.comco.scrappy.Shape

/**
 * Composes pickers of comparable types together.
 */
case class AndThenPicker[-A <: Shape.Any: TypeTag, B <: Shape.Any: TypeTag, +C <: Shape.Any: TypeTag](
  val first: Picker[A, B],
  val next: Picker[B, C])
    extends Picker[A, C] {
  require(first.targetType <:< next.sourceType)

  override def sourceType = first.sourceType
  override def targetType = next.targetType

  override def pickData(source: Data[A]) = next.pickData(first.pickData(source))

  override def pickOriginatedData(source: OriginatedData[A]) =
    next.pickOriginatedData(first.pickOriginatedData(source))
}