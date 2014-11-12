package com.github.comco.scrappy.picker

import com.github.comco.scrappy.Type
import com.github.comco.scrappy.data.Data
import com.github.comco.scrappy.originated_data.OriginatedData
import com.github.comco.scrappy.TopType

/**
 * Discards its source (leaving only the origin) and returns a constant.
 */
case class ConstPicker(val data: Data)
    extends BasePicker {
  val sourceType = TopType
  val targetType = data.datatype

  def doPickData(source: Data) = data

  def doPickOriginatedData(source: OriginatedData) =
    OriginatedData.from(data,
      source.origin.computedWithTargetType(targetType))
}