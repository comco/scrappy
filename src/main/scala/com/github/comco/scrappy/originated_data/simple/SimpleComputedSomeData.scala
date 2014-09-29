package com.github.comco.scrappy.originated_data.simple

import com.github.comco.scrappy.origin.Origin
import com.github.comco.scrappy.data.SomeData
import com.github.comco.scrappy.originated_data.OriginatedData
import com.github.comco.scrappy.originated_data.OriginatedSomeData

case class SimpleComputedSomeData(
  val data: SomeData,
  val origin: Origin,
  val value: OriginatedData)
    extends OriginatedSomeData