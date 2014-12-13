package com.github.comco.scrappy.picker

import com.github.comco.scrappy.PrimitiveType
import com.github.comco.scrappy.SeqType
import com.github.comco.scrappy.data.PrimitiveData
import com.github.comco.scrappy.data.SeqData
import com.github.comco.scrappy.originated_data.OriginatedPrimitiveData
import com.github.comco.scrappy.originated_data.OriginatedSeqData

/**
 * Lifts a raw fold function to a value.
 */
case class FoldPicker[A, R](f: Seq[A] => R)(
    implicit sourceElementType: PrimitiveType[A], 
    val targetType: PrimitiveType[R])
    extends BaseSeqPicker {
  
  def sourceType = SeqType(sourceElementType)
  
  def doPickData(source: SeqData): PrimitiveData[R] = {
    val rawArgs = source.elements.map(_.asInstanceOf[PrimitiveData[A]].value)
    val rawRes = f(rawArgs)
    PrimitiveData[R](rawRes)
  }
  
  def doPickOriginatedData(source: OriginatedSeqData): OriginatedPrimitiveData[R] = {
    val pickedData = doPickData(source.data)
    OriginatedPrimitiveData[R](pickedData, 
        source.origin.computedWithTargetType(pickedData.datatype))
  }
}