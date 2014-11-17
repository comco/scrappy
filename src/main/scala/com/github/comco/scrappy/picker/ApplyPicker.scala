package com.github.comco.scrappy.picker

import com.github.comco.scrappy.PrimitiveType
import com.github.comco.scrappy.data.PrimitiveData
import com.github.comco.scrappy.originated_data.OriginatedPrimitiveData
import com.github.comco.scrappy.data.Data
import com.github.comco.scrappy.originated_data.OriginatedData

/**
 * Picker for applying a raw scala function to an element
 */
case class ApplyPicker[A, R](f: A => R)(
  implicit val sourceType: PrimitiveType[A], val targetType: PrimitiveType[R])
    extends BasePicker[PrimitiveType[A], PrimitiveType[R]] {

  override def doPickData(source: Data[PrimitiveType[A]]): PrimitiveData[R] = {
    PrimitiveData(f(source.value))
  }

  override def doPickOriginatedData(source: OriginatedData[PrimitiveType[A]]): OriginatedPrimitiveData[R] = {
    OriginatedPrimitiveData(doPickData(source.data), source.origin.computedWithTargetType(targetType))
  }
}