package com.github.comco.scrappy.picker

import com.github.comco.scrappy.Type
import com.github.comco.scrappy.data.Data
import com.github.comco.scrappy.originated_data.OriginatedData

/**
 * Discards its source (leaving only the origin) and returns a constant.
 */
case class ConstPicker[TargetType >: Type.Nil <: Type.Any](val data: Data[TargetType])
    extends Picker[Type.Nil, TargetType] {
  val targetType = data.datatype

  def doPickData(source: Data.Nil) = data

  def doPickOriginatedData(source: OriginatedData.Nil) =
    OriginatedData.from(data,
      source.origin.computedWithTargetType(targetType))
}