package com.github.comco.scrappy.data

import com.github.comco.scrappy.TupleType
import com.github.comco.scrappy.data.simple.SimpleTupleData

abstract class TupleData extends Data.PackageSealed {
  require(datatype.length == coordinates.size,
    s"Invalid size of coordinates to construct a TupleData; expected: ${datatype.length}, actual: ${coordinates.size}.")
  require(datatype.coordinateTypes.zip(coordinates).forall {
    case (datatype, data) => data.datatype == datatype
  }, "Data coordinates types don't match tuple datatypes for TupleData construction.")

  def datatype: TupleType

  /**
   * The coordinates of this tuple data.
   */
  def coordinates: IndexedSeq[Data]

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
    datatype.hasCoordinate(position) && Data.isFilled(coordinates(position))
  }

  /**
   * Retrieves the coordinate of this data at some position.
   */
  def coordinate(position: Int): Data = {
    require(datatype.hasCoordinate(position),
      s"Coordinate position: $position is out of bounds for TupleType: $datatype")

    coordinates(position)
  }
}

object TupleData {
  def apply(datatype: TupleType, coordinates: IndexedSeq[Data]): TupleData = {
    SimpleTupleData(datatype, coordinates)
  }

  def apply(datatype: TupleType)(coordinates: Data*): TupleData = {
    TupleData(datatype, coordinates.toIndexedSeq)
  }

  def apply(coordinates: Data*): TupleData = {
    val datatype = TupleType(coordinates.map(_.datatype).toIndexedSeq)
    TupleData(datatype, coordinates.toIndexedSeq)
  }
}