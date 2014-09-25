package com.github.comco.scrappy.originated_data.simple

import com.github.comco.scrappy.OptionType
import com.github.comco.scrappy.Origin
import com.github.comco.scrappy.data.NoneData
import com.github.comco.scrappy.originated_data.OriginatedNoneData

case class SimpleNoneData(val origin: Origin)
    extends OriginatedNoneData {
  require(origin.targetType.isInstanceOf[OptionType],
    s"Origin: $origin doesn't have an option targetType.")

  def datatype = origin.targetType.asInstanceOf[OptionType]

  lazy val data = NoneData(datatype)
}