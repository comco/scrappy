package com.github.comco.scrappy.picker

import scala.IndexedSeq

import com.github.comco.scrappy.SeqType
import com.github.comco.scrappy.TupleType
import com.github.comco.scrappy.data.SeqData
import com.github.comco.scrappy.data.TupleData
import com.github.comco.scrappy.originated_data.OriginatedData
import com.github.comco.scrappy.originated_data.OriginatedSeqData
import com.github.comco.scrappy.originated_data.OriginatedTupleData

/**
 * Groups a sequence of objects into groups based on a picker.
 */
case class GroupPicker(by: Picker) extends BaseSeqPicker {
  def sourceType = SeqType(by.sourceType)
  def tupleType = TupleType(by.targetType, sourceType)
  def targetType = SeqType(tupleType)

  def doPickData(source: SeqData): SeqData = {
    val rawResultMap = source.elements.groupBy(by.pickData(_))
    val results = rawResultMap.toSeq.map {
      case (value, elements) =>
        TupleData(tupleType,
          IndexedSeq(value, SeqData(sourceType, elements)))
    }
    SeqData(targetType, results)
  }
  
  def doPickOriginatedData(source: OriginatedSeqData): OriginatedSeqData = {
    val rawResultMap = source.elements.groupBy(by.pickOriginatedData(_).data)
    val results = rawResultMap.toSeq.map {
      case (value, elements) =>
        val computedValue = OriginatedData.from(value, source)
        val computedData = SeqData(sourceType, elements.map(_.data))
        val computedOrigin = source.origin.computedWithTargetType(sourceType)
        val computedElements = OriginatedSeqData(computedData, computedOrigin, elements)
        val computedTupleData = TupleData(tupleType, IndexedSeq(value, computedData))
        val computedTupleOrigin = source.origin.computedWithTargetType(tupleType)
        OriginatedTupleData(computedTupleData, computedTupleOrigin, IndexedSeq(computedValue, computedElements))
    }
    OriginatedSeqData(doPickData(source.data), source.origin.computedWithTargetType(targetType), results)
  } 
}