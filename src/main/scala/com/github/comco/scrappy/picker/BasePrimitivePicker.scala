package com.github.comco.scrappy.picker

import com.github.comco.scrappy.PrimitiveType
import com.github.comco.scrappy.data.Data
import com.github.comco.scrappy.data.PrimitiveData
import com.github.comco.scrappy.originated_data.OriginatedData
import com.github.comco.scrappy.originated_data.OriginatedPrimitiveData

/**
 * Base class for primitive types pickers.
 */
abstract class BasePrimitivePicker[T] extends BasePicker {
  def sourceType: PrimitiveType[T]
  
  def doPickData(source: Data) = {
    doPickData(source.asInstanceOf[PrimitiveData[T]])
  }
  
  def doPickData(source: PrimitiveData[T]): Data
  
  def doPickOriginatedData(source: OriginatedData) = {
    doPickOriginatedData(source.asInstanceOf[OriginatedPrimitiveData[T]])
  }
  
  def doPickOriginatedData(source: OriginatedPrimitiveData[T]): OriginatedData
}