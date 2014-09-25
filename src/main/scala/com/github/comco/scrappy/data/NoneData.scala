package com.github.comco.scrappy.data

import com.github.comco.scrappy.OptionType
import com.github.comco.scrappy.data.simple.SimpleNoneData

abstract class NoneData extends OptionData.Base {
  def isSome = false
}

object NoneData {
  def apply(datatype: OptionType): NoneData =
    SimpleNoneData(datatype)
}