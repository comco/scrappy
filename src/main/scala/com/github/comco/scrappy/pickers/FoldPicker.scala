package com.github.comco.scrappy.pickers

import com.github.comco.scrappy._

/**
 * Lifts a raw fold function to a value.
 */
case class FoldPicker[A, R](f: Seq[A] => R)(
    implicit sourceElementType: PrimitiveType[A], 
    val targetType: PrimitiveType[R])
    extends BaseSeqPicker {
  
  def sourceType = SeqType(sourceElementType)
  
  def doPickData(source: DataDomain.SeqData): DataDomain.PrimitiveData[R] = {
    val rawArgs = source.elements.map(_.asInstanceOf[DataDomain.PrimitiveData[A]].value)
    val rawRes = f(rawArgs)
    DataDomain.PrimitiveData[R](rawRes)
  }
  
  def doPickOriginatedData(source: OriginatedDataDomain.SeqData): OriginatedDataDomain.PrimitiveData[R] = {
    val pickedData = doPickData(source.data)
    OriginatedDataDomain.PrimitiveData[R](pickedData, 
        source.origin.computedWithTargetType(pickedData.datatype))
  }
}