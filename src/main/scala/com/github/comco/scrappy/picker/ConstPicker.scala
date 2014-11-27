package com.github.comco.scrappy.picker

import scala.reflect.runtime.universe._

import com.github.comco.scrappy.Type
import com.github.comco.scrappy.data.Data
import com.github.comco.scrappy.originated_data.OriginatedData
import com.github.comco.scrappy.Shape

/**
 * Discards its source (leaving only the origin) and returns a constant.
 */
case class ConstPicker[TargetShape <: Shape.Any: TypeTag](val data: Data[TargetShape])
    extends Picker[Shape.Nil, TargetShape] {
  val targetType = data.datatype

  def doPickData(source: Data.Nil) = data

  def doPickOriginatedData(source: OriginatedData.Nil) =
    OriginatedData.from(data,
      source.origin.computedWithTargetType(targetType))
}