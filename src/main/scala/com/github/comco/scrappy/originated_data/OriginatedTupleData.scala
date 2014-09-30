package com.github.comco.scrappy.originated_data

import com.github.comco.scrappy.TupleType
import com.github.comco.scrappy.data.TupleData
import com.github.comco.scrappy.origin.Origin
import com.github.comco.scrappy.originated_data.simple.SimpleComputedTupleData
import com.github.comco.scrappy.originated_data.simple.SimpleOriginalTupleData

abstract class OriginatedTupleData extends OriginatedData.PackageSealed {
  def data: TupleData
  def datatype: TupleType = data.datatype

  /**
   * The coordinates of this tuple data.
   */
  def coordinates: IndexedSeq[OriginatedData]

  /**
   * The length (arity) of this tuple data.
   */
  def length: Int = datatype.length

  /**
   * Checks if this tuple data has a coordinate at some position.
   * Positions of optional coordinates which are not filled-in
   * are regarded as not occupied.
   */
  def isOccupied(position: Int): Boolean = {
    datatype.hasCoordinate(position) && OriginatedData.isFilled(coordinates(position))
  }

  /**
   * Retrieves the coordinate of this data at some position.
   */
  def coordinate(position: Int): OriginatedData = {
    require(datatype.hasCoordinate(position),
      s"Coordinate position: $position is out of bounds for TupleType: $datatype")

    coordinates(position)
  }
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