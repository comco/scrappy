package com.github.comco.scrappy.picker

import com.github.comco.scrappy.PrimitiveType
import com.github.comco.scrappy.data.Data
import com.github.comco.scrappy.data.PrimitiveData
import com.github.comco.scrappy.originated_data.OriginatedData
import com.github.comco.scrappy.originated_data.OriginatedPrimitiveData

/**
 * Base class for primitive types pickers.
 */
abstract class BasePrimitivePicker[T] extends Picker {
  def sourceType: PrimitiveType[T]
  
  def pickData(source: Data) = {
    require(source.datatype == sourceType)
    doPickData(source.asInstanceOf[PrimitiveData[T]])
  } ensuring (_.datatype == targetType)
  
  def doPickData(source: PrimitiveData[T]): Data
  
  def pickOriginatedData(source: OriginatedData) = {
    require(source.datatype == sourceType)
    doPickOriginatedData(source.asInstanceOf[OriginatedPrimitiveData[T]])
  } ensuring (_.datatype == targetType)
  
  def doPickOriginatedData(source: OriginatedPrimitiveData[T]): OriginatedData
}