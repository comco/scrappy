package com.github.comco.scrappy.originated_data

import com.github.comco.scrappy.OptionType
import com.github.comco.scrappy.data.OptionData

sealed abstract class OriginatedOptionData extends OriginatedData.PackageSealed {
  def datatype: OptionType
  def data: OptionData
  
  def isSome = data.isSome
}

object OriginatedOptionData {
  private[originated_data] abstract class PackageSealed extends OriginatedOptionData
}