package com.github.comco.scrappy.originated_data

import com.github.comco.scrappy.OptionType
import com.github.comco.scrappy.data.OptionData

sealed abstract class OriginatedOptionData extends OriginatedData.Base {
  def datatype: OptionType
  def data: OptionData
}

object OriginatedOptionData {
  private[originated_data] abstract class Base extends OriginatedOptionData
}