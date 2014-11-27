package com.github.comco.scrappy.picker

import scala.reflect.runtime.universe._

import com.github.comco.scrappy.TupleType
import com.github.comco.scrappy.data.Data
import com.github.comco.scrappy.data.TupleData
import com.github.comco.scrappy.originated_data.OriginatedData
import com.github.comco.scrappy.originated_data.OriginatedTupleData
import com.github.comco.scrappy.Type
import com.github.comco.scrappy.Shape

case class TuplePicker[+SourceShape <: Shape.Any: TypeTag](
  coordinatePickers: IndexedSeq[Picker[SourceShape, Shape.Any]])
    extends BasePicker[SourceShape, Shape.Tuple] {

  require(coordinatePickers.size > 0, "A TuplePicker cannot be created with zero pickers.")
  require(coordinatePickers.forall(_.sourceType == sourceType),
    s"All coordinate pickers: $coordinatePickers should have the same source type.")

  def sourceType = coordinatePickers.head.sourceType
  lazy val targetType = TupleType(coordinatePickers.map(_.targetType))

  def doPickData(source: Data[SourceShape]): Data.Tuple = {
    TupleData(targetType, coordinatePickers.map(_.pickData(source)))
  }

  def doPickOriginatedData(source: OriginatedData[SourceShape]) = {
    val data = doPickData(source.data)
    val origin = source.origin.computedWithTargetType(targetType)
    val coordinates = coordinatePickers.map(_.pickOriginatedData(source))
    OriginatedTupleData.computed(data, origin, coordinates)
  }
}