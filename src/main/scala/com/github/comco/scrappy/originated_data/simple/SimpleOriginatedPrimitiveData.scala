package com.github.comco.scrappy.originated_data.simple

import scala.reflect.runtime.universe.TypeTag

import com.github.comco.scrappy.origin.Origin
import com.github.comco.scrappy.data.PrimitiveData
import com.github.comco.scrappy.originated_data.OriginatedPrimitiveData

case class SimpleOriginatedPrimitiveData[T: TypeTag](
  val data: PrimitiveData[T],
  val origin: Origin)
    extends OriginatedPrimitiveData[T]