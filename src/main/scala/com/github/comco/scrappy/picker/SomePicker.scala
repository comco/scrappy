package com.github.comco.scrappy.picker

import com.github.comco.scrappy.OptionType
import com.github.comco.scrappy.data.OptionData
import com.github.comco.scrappy.originated_data.OriginatedOptionData
import com.github.comco.scrappy.data.SomeData
import com.github.comco.scrappy.originated_data.OriginatedSomeData

case class SomePicker(val sourceType: OptionType)
    extends BaseOptionPicker {

  def targetType = sourceType.someType

  def doPickData(source: OptionData) = source match {
    case source: SomeData => source.value
    case _ => throw new IllegalArgumentException("SomePicker cannot pick NoneData")
  }

  def doPickOriginatedData(source: OriginatedOptionData) = source match {
    case source: OriginatedSomeData => source.value
    case _ => throw new IllegalArgumentException("SomePicker cannot pick NoneData")
  }
}