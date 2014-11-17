package com.github.comco.scrappy.originated_data.simple

import com.github.comco.scrappy.origin.Origin
import com.github.comco.scrappy.data.SomeData
import com.github.comco.scrappy.originated_data.OriginatedData
import com.github.comco.scrappy.originated_data.OriginatedSomeData
import com.github.comco.scrappy.Type

case class SimpleComputedSomeData(
  val data: SomeData,
  val origin: Origin,
  val value: OriginatedData[Type[Any]])
    extends OriginatedSomeData