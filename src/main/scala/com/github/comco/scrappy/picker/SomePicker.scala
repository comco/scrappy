package com.github.comco.scrappy.picker

import com.github.comco.scrappy.Type
import com.github.comco.scrappy.OptionType
import com.github.comco.scrappy.data.OptionData
import com.github.comco.scrappy.originated_data.OriginatedOptionData
import com.github.comco.scrappy.data.SomeData
import com.github.comco.scrappy.originated_data.OriginatedSomeData
import com.github.comco.scrappy.data.Data
import com.github.comco.scrappy.originated_data.OriginatedData

case class SomePicker(val sourceType: OptionType)
    extends BasePicker[OptionType, Type.Any] {

  def targetType = sourceType.someType

  def doPickData(source: Data.Option) = source match {
    case source: SomeData => source.value
    case _ => throw new IllegalArgumentException("SomePicker cannot pick NoneData")
  }

  def doPickOriginatedData(source: OriginatedData.Option) = source match {
    case source: OriginatedSomeData => source.value
    case _ => throw new IllegalArgumentException("SomePicker cannot pick NoneData")
  }
}