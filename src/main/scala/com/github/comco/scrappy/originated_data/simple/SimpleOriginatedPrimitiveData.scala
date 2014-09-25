package com.github.comco.scrappy.originated_data.simple

import com.github.comco.scrappy.Origin
import com.github.comco.scrappy.data.PrimitiveData
import com.github.comco.scrappy.originated_data.OriginatedPrimitiveData

case class SimpleOriginatedPrimitiveData[T](
  val data: PrimitiveData[T],
  val origin: Origin)
    extends OriginatedPrimitiveData[T]