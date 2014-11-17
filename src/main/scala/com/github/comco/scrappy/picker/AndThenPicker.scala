package com.github.comco.scrappy.picker

import com.github.comco.scrappy.data.Data
import com.github.comco.scrappy.originated_data.OriginatedData
import com.github.comco.scrappy.Type

/**
 * Composes pickers of comparable types together.
 */
case class AndThenPicker[A, B, C](val first: Picker[Type[A], Type[B]], val next: Picker[Type[B], Type[C]])
    extends Picker[Type[A], Type[C]] {
  require(first.targetType <:< next.sourceType)

  override def sourceType = first.sourceType
  override def targetType = next.targetType

  override def pickData(source: Data[Type[A]]) = next.pickData(first.pickData(source))

  override def pickOriginatedData(source: OriginatedData[Type[A]]) =
    next.pickOriginatedData(first.pickOriginatedData(source))
}