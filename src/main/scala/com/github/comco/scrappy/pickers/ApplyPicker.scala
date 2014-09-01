package com.github.comco.scrappy.pickers

import com.github.comco.scrappy._

/**
 * Picker for applying a raw scala function to an element
 */
case class ApplyPicker[A, R](f: A => R)(
    implicit val sourceType: PrimitiveType[A], val targetType: PrimitiveType[R]) 
    extends BasePrimitivePicker[A] {
  def doPickData(source: DataDomain.PrimitiveData[A]): DataDomain.PrimitiveData[R] = {
    DataDomain.PrimitiveData[R](f(source.value))
  }
  
  def doPickOriginatedData(source: OriginatedDataDomain.PrimitiveData[A]): OriginatedDataDomain.PrimitiveData[R] = {
    OriginatedDataDomain.PrimitiveData(doPickData(source.data), source.origin.computedWithTargetType(targetType))
  }
}