package com.github.comco.scrappy.pickers

import com.github.comco.scrappy._

case class TuplePicker(coordinatePickers: IndexedSeq[Picker]) extends BasePicker {
  require(coordinatePickers.size > 0, "A TuplePicker cannot be created with zero pickers.")
  require(coordinatePickers.forall(_.sourceType == sourceType), 
      s"All coordinate pickers: $coordinatePickers should have the same source type.")
  
  def sourceType = coordinatePickers.head.sourceType
  lazy val targetType = TupleType(coordinatePickers.map(_.targetType))
  
  def doPickData(source: DataDomain.Data) = {
    DataDomain.TupleData(targetType, coordinatePickers.map(_.pickData(source)))
  }
  
  def doPickOriginatedData(source: OriginatedDataDomain.Data) = {
    val pickedSource = doPickData(source.data)
    OriginatedDataDomain.mkDataOriginatedFrom(pickedSource, source.origin.computedWithTargetType(targetType))
  }
}

object TuplePicker {
  def apply(firstCoordinatePicker: Picker, nextCoordinatePickers: Picker*): TuplePicker =
    TuplePicker((firstCoordinatePicker +: nextCoordinatePickers).toIndexedSeq)
}