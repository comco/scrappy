package com.github.comco.scrappy.originated_data.simple

import com.github.comco.scrappy.origin.Origin
import com.github.comco.scrappy.data.NoneData
import com.github.comco.scrappy.originated_data.OriginatedNoneData
import com.github.comco.scrappy.OptionalType

case class SimpleOriginatedNoneData(val origin: Origin)
  extends OriginatedNoneData