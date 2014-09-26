package com.github.comco.scrappy.picker

import com.github.comco.scrappy.OptionType
import com.github.comco.scrappy.data.Data
import com.github.comco.scrappy.data.OptionData
import com.github.comco.scrappy.originated_data.OriginatedData
import com.github.comco.scrappy.originated_data.OriginatedOptionData

abstract class BaseOptionPicker extends Picker {
  def sourceType: OptionType
  
  def pickData(source: Data) = {
    require(source.datatype == sourceType)
    doPickData(source.asInstanceOf[OptionData])
  } ensuring (_.datatype == targetType)
  
  def doPickData(source: OptionData): Data
  
  def pickOriginatedData(source: OriginatedData) = {
    require(source.datatype == sourceType)
    doPickOriginatedData(source.asInstanceOf[OriginatedOptionData])
  } ensuring (_.datatype == targetType)
  
  def doPickOriginatedData(source: OriginatedOptionData): OriginatedData
}
