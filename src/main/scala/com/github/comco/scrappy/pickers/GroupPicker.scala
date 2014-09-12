package com.github.comco.scrappy.pickers

import com.github.comco.scrappy._

/**
 * Groups a sequence of objects into groups based on a picker.
 */
case class GroupPicker(by: Picker) extends BaseSeqPicker {
  def sourceType = SeqType(by.sourceType)
  def tupleType = TupleType(by.targetType, sourceType)
  def targetType = SeqType(tupleType)

  def doPickData(source: DataDomain.SeqData): DataDomain.SeqData = {
    val rawResultMap = source.elements.groupBy(by.pickData(_))
    val results = rawResultMap.toSeq.map {
      case (value, elements) =>
        DataDomain.TupleData(tupleType,
          IndexedSeq(value, DataDomain.SeqData(sourceType, elements)))
    }
    DataDomain.SeqData(targetType, results)
  }
  
  def doPickOriginatedData(source: OriginatedDataDomain.SeqData): OriginatedDataDomain.SeqData = {
    val rawResultMap = source.elements.groupBy(by.pickOriginatedData(_).data)
    val results = rawResultMap.toSeq.map {
      case (value, elements) =>
        val computedValue = OriginatedDataDomain.mkDataComputedFrom(value, source)
        val computedData = DataDomain.SeqData(sourceType, elements.map(_.data))
        val computedOrigin = source.origin.computedWithTargetType(sourceType)
        val computedElements = OriginatedDataDomain.ComputedSeqData(computedData, computedOrigin, elements)
        val computedTupleData = DataDomain.TupleData(tupleType, IndexedSeq(value, computedData))
        val computedTupleOrigin = source.origin.computedWithTargetType(tupleType)
        OriginatedDataDomain.ComputedTupleData(computedTupleData, computedTupleOrigin, IndexedSeq(computedValue, computedElements))
    }
    OriginatedDataDomain.ComputedSeqData(doPickData(source.data), source.origin.computedWithTargetType(targetType), results)
  } 
}