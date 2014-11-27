package com.github.comco.scrappy.picker

import scala.reflect.runtime.universe._

import com.github.comco.scrappy.PrimitiveType
import com.github.comco.scrappy.data.PrimitiveData
import com.github.comco.scrappy.originated_data.OriginatedPrimitiveData
import com.github.comco.scrappy.data.Data
import com.github.comco.scrappy.originated_data.OriginatedData
import com.github.comco.scrappy.Shape

/**
 * Picker for applying a raw scala function to an element
 */
case class ApplyPicker[-RawArgumentType: TypeTag, +RawResultType: TypeTag](f: RawArgumentType => RawResultType)(
  implicit val sourceType: PrimitiveType[RawArgumentType], val targetType: PrimitiveType[RawResultType])
    extends BasePicker[Shape.Primitive[RawArgumentType], Shape.Primitive[RawResultType]] {

  def doPickData(source: Data[Shape.Primitive[RawArgumentType]]): PrimitiveData[RawResultType] = {
    PrimitiveData(f(source.raw))
  }

  def doPickOriginatedData(source: OriginatedData[Shape.Primitive[RawArgumentType]]): OriginatedPrimitiveData[RawResultType] = {
    OriginatedPrimitiveData(doPickData(source.data), source.origin.computedWithTargetType(targetType))
  }
}