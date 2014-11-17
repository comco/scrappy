package com.github.comco.scrappy.picker

import com.github.comco.scrappy.TupleType
import com.github.comco.scrappy.data.Data
import com.github.comco.scrappy.data.TupleData
import com.github.comco.scrappy.originated_data.OriginatedData
import com.github.comco.scrappy.originated_data.OriginatedTupleData
import com.github.comco.scrappy.Type

case class TuplePicker[SourceType >: Type.Nil <: Type.Any](
  coordinatePickers: IndexedSeq[Picker[SourceType, Type.Any]])
    extends BasePicker[SourceType, Type.Tuple] {
  
  require(coordinatePickers.size > 0, "A TuplePicker cannot be created with zero pickers.")
  require(coordinatePickers.forall(_.sourceType == sourceType),
    s"All coordinate pickers: $coordinatePickers should have the same source type.")

  def sourceType = coordinatePickers.head.sourceType
  lazy val targetType = TupleType(coordinatePickers.map(_.targetType))

  def doPickData(source: Data[SourceType]): Data.Tuple = {
    TupleData(targetType, coordinatePickers.map(_.pickData(source)))
  }

  def doPickOriginatedData(source: OriginatedData[SourceType]) = {
    val data = doPickData(source.data)
    val origin = source.origin.computedWithTargetType(targetType)
    val coordinates = coordinatePickers.map(_.pickOriginatedData(source))
    OriginatedTupleData.computed(data, origin, coordinates)
  }
}

object TuplePicker {
  def apply[SourceType >: Type.Nil <: Type.Any, A](
      firstCoordinatePicker: Picker[SourceType, Type.Any], 
      nextCoordinatePickers: Picker[SourceType, Type.Any]*): TuplePicker[SourceType] =
    TuplePicker[SourceType]((firstCoordinatePicker +: nextCoordinatePickers).toIndexedSeq)
}