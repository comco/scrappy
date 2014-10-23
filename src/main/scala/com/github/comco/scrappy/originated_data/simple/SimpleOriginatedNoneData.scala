package com.github.comco.scrappy.originated_data.simple

import com.github.comco.scrappy.OptionType
import com.github.comco.scrappy.origin.Origin
import com.github.comco.scrappy.data.NoneData
import com.github.comco.scrappy.originated_data.OriginatedNoneData

case class SimpleOriginatedNoneData private(val origin: Origin, val datatype: OptionType)
    extends OriginatedNoneData {
  def data = NoneData(datatype)
}

object SimpleOriginatedNoneData {
  def apply(origin: Origin): SimpleOriginatedNoneData = {
    require(origin.targetType.isInstanceOf[OptionType],
      s"Origin: $origin doesn't have an option targetType.")
      SimpleOriginatedNoneData(origin, origin.targetType.asInstanceOf[OptionType])
  }
}