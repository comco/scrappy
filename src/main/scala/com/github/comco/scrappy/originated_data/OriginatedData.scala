package com.github.comco.scrappy.originated_data

import com.github.comco.scrappy.OptionType
import com.github.comco.scrappy.PrimitiveType
import com.github.comco.scrappy.SeqType
import com.github.comco.scrappy.StructType
import com.github.comco.scrappy.TupleType
import com.github.comco.scrappy.Type
import com.github.comco.scrappy.data.Data
import com.github.comco.scrappy.data.NoneData
import com.github.comco.scrappy.data.OptionData
import com.github.comco.scrappy.data.PrimitiveData
import com.github.comco.scrappy.data.SeqData
import com.github.comco.scrappy.data.SomeData
import com.github.comco.scrappy.data.StructData
import com.github.comco.scrappy.data.TupleData
import com.github.comco.scrappy.origin.Origin
import com.github.comco.scrappy.origin.OriginalOrigin
import com.github.comco.scrappy.originated_data.simple.SimpleComputedSeqData
import com.github.comco.scrappy.originated_data.simple.SimpleComputedSomeData
import com.github.comco.scrappy.originated_data.simple.SimpleComputedStructData
import com.github.comco.scrappy.originated_data.simple.SimpleComputedTupleData
import com.github.comco.scrappy.originated_data.simple.SimpleOriginalSeqData
import com.github.comco.scrappy.originated_data.simple.SimpleOriginalSomeData
import com.github.comco.scrappy.originated_data.simple.SimpleOriginalStructData
import com.github.comco.scrappy.originated_data.simple.SimpleOriginalTupleData
import com.github.comco.scrappy.originated_data.simple.SimpleOriginatedNoneData
import com.github.comco.scrappy.originated_data.simple.SimpleOriginatedPrimitiveData
import com.github.comco.scrappy.pointer.SelfPointer

sealed abstract class OriginatedData {
  require(data.datatype == origin.targetType,
    s"Datatype of data: $data and targetType of origin: $origin does not match.")

  /**
   * The type of this originated data.
   */
  def datatype: Type

  def data: Data
  def origin: Origin
}

object OriginatedData {
  /**
   * Checks if data contains a value.
   * OptionData in case of none data doesn't contain a value.
   */
  def isFilled(data: OriginatedData): Boolean = data match {
    case data: OriginatedOptionData => data.isSome
    case _ => true
  }

  def from(data: Data, origin: Origin): OriginatedData = data match {
    case data: PrimitiveData[t] => OriginatedPrimitiveData[t](data, origin)
    case data: TupleData => OriginatedTupleData.original(data, origin)
    case data: StructData => OriginatedStructData.original(data, origin)
    case data: SeqData => OriginatedSeqData.original(data, origin)
    case data: SomeData => OriginatedSomeData.original(data, origin)
    case data: NoneData => SimpleOriginatedNoneData(origin)
  }

  def from(data: Data, source: OriginatedData): OriginatedData = {
    from(data, source.origin.computedWithTargetType(data.datatype))
  }

  def fromSelf(data: Data): OriginatedData = {
    from(data, OriginalOrigin(SelfPointer(data.datatype)))
  }
}

abstract class OriginatedPrimitiveData[T] extends OriginatedData {
  def datatype: PrimitiveType[T] = data.datatype
  def data: PrimitiveData[T]
  def value: T = data.value
}

object OriginatedPrimitiveData {
  def apply[T](data: PrimitiveData[T], origin: Origin): OriginatedPrimitiveData[T] = {
    SimpleOriginatedPrimitiveData(data, origin)
  }
}

sealed abstract class OriginatedOptionData extends OriginatedData {
  def datatype: OptionType
  def data: OptionData
  
  def isSome = data.isSome
}

abstract class OriginatedNoneData extends OriginatedOptionData

abstract class OriginatedSomeData extends OriginatedOptionData {
  def datatype = data.datatype
  def data: SomeData

  def value: OriginatedData
}

object OriginatedSomeData {
  def original(data: SomeData, origin: Origin): OriginatedSomeData = {
    SimpleOriginalSomeData(data, origin)
  }
  
  def computed(data: SomeData, origin: Origin, value: OriginatedData): OriginatedSomeData = {
    SimpleComputedSomeData(data, origin, value)
  }
}

abstract class OriginatedTupleData extends OriginatedData {
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

abstract class OriginatedStructData extends OriginatedData {
  def datatype: StructType = data.datatype
  def data: StructData

  /**
   * The features of this originated struct data.
   */
  def features: Map[String, OriginatedData]
  
  /**
   * Checks if this struct data has a feature with some name.
   * Names of optional features which are not filled-in
   * are regarded as not occupied.
   */
  def isOccupied(name: String): Boolean = {
    features.contains(name) && OriginatedData.isFilled(features(name))
  }

  /**
   * Retrieves the feature of this struct data with some name.
   */
  def feature(name: String): OriginatedData = {
    require(datatype.hasFeature(name),
      s"StructData doesn't contain a feature named: $name")

    features(name)
  }
}

object OriginatedStructData {
  def original(data: StructData, origin: Origin): OriginatedStructData =
    SimpleOriginalStructData(data, origin)

  def computed(data: StructData,
    origin: Origin,
    features: Map[String, OriginatedData]): OriginatedStructData =
    SimpleComputedStructData(data, origin, features)
}

abstract class OriginatedSeqData extends OriginatedData {
  def datatype: SeqType = data.datatype
  def data: SeqData

  /**
   * The elements of this originated seq data.
   */
  def elements: Seq[OriginatedData]

  /**
   * Checks if this struct data has a feature with some name.
   * Names of optional features which are not filled-in
   * are regarded as not occupied.
   */
  def isOccupied(index: Int): Boolean = {
    0 <= index && index < length && OriginatedData.isFilled(elements(index))
  }
  
  /**
   * Retrieves the element of this seq data at some index.
   */
  def element(index: Int): OriginatedData = {
    require(0 <= index && index < length,
      s"Index: $index is out of bounds for SeqData with length: $length.")

    elements(index)
  }
  
  /**
   * The length of this seq data.
   */
  def length: Int = elements.length
}

object OriginatedSeqData {
  def original(data: SeqData, origin: Origin): OriginatedSeqData =
    SimpleOriginalSeqData(data, origin)

  def computed(data: SeqData,
    origin: Origin,
    elements: Seq[OriginatedData]): OriginatedSeqData =
    SimpleComputedSeqData(data, origin, elements)

  def apply(data: SeqData, origin: Origin): OriginatedSeqData =
    original(data, origin)

  def apply(data: SeqData,
    origin: Origin,
    elements: Seq[OriginatedData]): OriginatedSeqData =
    computed(data, origin, elements)
}

