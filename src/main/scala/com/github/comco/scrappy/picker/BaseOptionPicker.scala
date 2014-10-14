package com.github.comco.scrappy.picker

import com.github.comco.scrappy.OptionType
import com.github.comco.scrappy.data.Data
import com.github.comco.scrappy.data.OptionData
import com.github.comco.scrappy.originated_data.OriginatedData
import com.github.comco.scrappy.originated_data.OriginatedOptionData

abstract class BaseOptionPicker extends BasePicker {
  def sourceType: OptionType
  
  def doPickData(source: Data) = {
    doPickData(source.asInstanceOf[OptionData])
  }
  
  def doPickData(source: OptionData): Data
  
  def doPickOriginatedData(source: OriginatedData) = {
    doPickOriginatedData(source.asInstanceOf[OriginatedOptionData])
  }
  
  def doPickOriginatedData(source: OriginatedOptionData): OriginatedData
}
