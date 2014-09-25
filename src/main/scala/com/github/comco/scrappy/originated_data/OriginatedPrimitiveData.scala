package com.github.comco.scrappy.originated_data

import com.github.comco.scrappy.PrimitiveType
import com.github.comco.scrappy.data.PrimitiveData

abstract class OriginatedPrimitiveData[T] extends OriginatedData.Base {
  def datatype: PrimitiveType[T] = data.datatype
  def data: PrimitiveData[T]
}