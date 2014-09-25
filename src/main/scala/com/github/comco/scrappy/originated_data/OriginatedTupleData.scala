package com.github.comco.scrappy.originated_data

import com.github.comco.scrappy.Origin
import com.github.comco.scrappy.TupleType
import com.github.comco.scrappy.data.TupleData
import com.github.comco.scrappy.originated_data.simple.SimpleComputedTupleData
import com.github.comco.scrappy.originated_data.simple.SimpleOriginalTupleData

abstract class OriginatedTupleData extends OriginatedData.Base {
  def data: TupleData
  def datatype: TupleType = data.datatype

  def coordinates: IndexedSeq[OriginatedData]
}

object OriginatedTupleData {
  def original(data: TupleData, origin: Origin): OriginatedTupleData =
    SimpleOriginalTupleData(data, origin)

  def computed(data: TupleData, 
    origin: Origin, 
    coordinates: IndexedSeq[OriginatedData]): OriginatedTupleData =
    SimpleComputedTupleData(data, origin, coordinates)

  def apply(data: TupleData, origin: Origin): OriginatedTupleData =
    original(data, origin)

  def apply(data: TupleData,
    origin: Origin,
    coordinates: IndexedSeq[OriginatedData]): OriginatedTupleData =
    computed(data, origin, coordinates)

  def apply(datatype: TupleType,
    origin: Origin,
    coordinates: IndexedSeq[OriginatedData]): OriginatedTupleData = {
    val data = TupleData(datatype, coordinates map (_.data))
    computed(data, origin, coordinates)
  }
  
  def unapply(that: OriginatedTupleData) =
    Some((that.data, that.origin, that.coordinates))
}