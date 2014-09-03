package com.github.comco.scrappy.pickers.ordering

import com.github.comco.scrappy._

case class OrderPicker(val by: Picker, val strategy: OrderingStrategy) extends BaseSeqPicker {
  require(by.targetType == strategy.datatype)
  
  def sourceType: SeqType = SeqType(by.sourceType)
  def targetType: SeqType = sourceType
  
  def doPickData(source: DataDomain.SeqData): DataDomain.SeqData = {
    val sortedElements = source.elements.sortBy(by.pickData(_))(strategy.dataOrdering)
    
    DataDomain.SeqData(targetType, sortedElements)
  }
  
  def doPickOriginatedData(source: OriginatedDataDomain.SeqData): OriginatedDataDomain.SeqData = {
    val sortedData = doPickData(source.data)
    val computedOrigin = source.origin.computed
    val sortedElements = source.elements.sortBy(by.pickOriginatedData(_))(strategy.originatedDataOrdering)
    
    OriginatedDataDomain.ComputedSeqData(sortedData, computedOrigin, sortedElements)
  }
}