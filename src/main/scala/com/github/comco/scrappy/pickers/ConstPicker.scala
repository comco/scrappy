package com.github.comco.scrappy.pickers

import com.github.comco.scrappy._

/**
 * Discards its source (leaving only the origin) and returns a constant.
 */
case class ConstPicker(val sourceType: Type, val data: DataDomain.Data) 
  extends BasePicker {
  val targetType = data.datatype
  
  def doPickData(source: DataDomain.Data) = data
  
  def doPickOriginatedData(source: OriginatedDataDomain.Data) =
    OriginatedDataDomain.mkDataOriginatedFrom(data, 
        source.origin.computedWithTargetType(targetType))
}