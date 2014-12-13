package com.github.comco.scrappy.data.simple

import com.github.comco.scrappy.PrimitiveType
import com.github.comco.scrappy.data.PrimitiveData

case class SimplePrimitiveData[T](val value: T)(
  implicit val datatype: PrimitiveType[T])
    extends PrimitiveData[T]