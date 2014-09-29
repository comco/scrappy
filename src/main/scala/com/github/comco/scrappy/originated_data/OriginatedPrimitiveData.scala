package com.github.comco.scrappy.originated_data

import com.github.comco.scrappy.PrimitiveType
import com.github.comco.scrappy.data.PrimitiveData
import com.github.comco.scrappy.originated_data.simple.SimpleOriginatedPrimitiveData
import com.github.comco.scrappy.origin.Origin

abstract class OriginatedPrimitiveData[T] extends OriginatedData.Base {
  def datatype: PrimitiveType[T] = data.datatype
  def data: PrimitiveData[T]
  def value: T = data.value
}

object OriginatedPrimitiveData {
  def apply[T](data: PrimitiveData[T], origin: Origin): OriginatedPrimitiveData[T] = {
    SimpleOriginatedPrimitiveData(data, origin)
  }
}