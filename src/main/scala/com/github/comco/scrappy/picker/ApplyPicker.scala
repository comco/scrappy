package com.github.comco.scrappy.picker

import com.github.comco.scrappy.PrimitiveType
import com.github.comco.scrappy.data.PrimitiveData
import com.github.comco.scrappy.originated_data.OriginatedPrimitiveData

/**
 * Picker for applying a raw scala function to an element
 */
case class ApplyPicker[A, R](f: A => R)(
    implicit val sourceType: PrimitiveType[A], val targetType: PrimitiveType[R]) 
    extends BasePrimitivePicker[A] {
  def doPickData(source: PrimitiveData[A]): PrimitiveData[R] = {
    PrimitiveData(f(source.value))
  }
  
  def doPickOriginatedData(source: OriginatedPrimitiveData[A]): OriginatedPrimitiveData[R] = {
    OriginatedPrimitiveData(doPickData(source.data), source.origin.computedWithTargetType(targetType))
  }
}