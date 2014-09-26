package com.github.comco.scrappy.picker

import com.github.comco.scrappy.Type
import com.github.comco.scrappy.data.Data
import com.github.comco.scrappy.originated_data.OriginatedData

/**
 * Discards its source (leaving only the origin) and returns a constant.
 */
case class ConstPicker(val sourceType: Type, val data: Data) 
  extends BasePicker {
  val targetType = data.datatype
  
  def doPickData(source: Data) = data
  
  def doPickOriginatedData(source: OriginatedData) =
    OriginatedData.from(data, 
        source.origin.computedWithTargetType(targetType))
}