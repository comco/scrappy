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
import com.github.comco.scrappy.originated_data.simple.SimpleOriginatedNoneData
import com.github.comco.scrappy.origin.OriginalOrigin
import com.github.comco.scrappy.pointer.SelfPointer
import com.github.comco.scrappy.origin.ComputedOrigin

/**
 * Base class for scrappy originated data - a piece of data with an origin.
 */
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

  /**
   * Methods for constructing originated data.
   */
  def from[T](data: PrimitiveData[T], origin: Origin) = OriginatedPrimitiveData[T](data, origin)
  def from(data: TupleData, origin: Origin) = OriginatedTupleData.original(data, origin)
  def from(data: StructData, origin: Origin) = OriginatedStructData.original(data, origin)
  def from(data: SeqData, origin: Origin) = OriginatedSeqData.original(data, origin)
  def from(data: SomeData, origin: Origin) = OriginatedSomeData.original(data, origin)
  def from(data: NoneData, origin: Origin) = OriginatedNoneData.simple(origin)

  def from(data: OptionData, origin: Origin): OriginatedOptionData = data match {
    case data: SomeData => from(data, origin)
    case data: NoneData => from(data, origin)
  }

  def from(data: Data, origin: Origin): OriginatedData = data match {
    case data: PrimitiveData[t] => from(data, origin)
    case data: TupleData => from(data, origin)
    case data: StructData => from(data, origin)
    case data: SeqData => from(data, origin)
    case data: OptionData => from(data, origin)
  }

  /**
   * Methods for constructing originated data by computing the target type.
   */
  private def mkComputedOrigin(data: Data, source: OriginatedData): ComputedOrigin = {
    source.origin.computedWithTargetType(data.datatype)
  }

  def from[T](data: PrimitiveData[T], source: OriginatedData): OriginatedPrimitiveData[T] = from(data, mkComputedOrigin(data, source))
  def from(data: TupleData, source: OriginatedData): OriginatedTupleData = from(data, mkComputedOrigin(data, source))
  def from(data: StructData, source: OriginatedData): OriginatedStructData = from(data, mkComputedOrigin(data, source))
  def from(data: SeqData, source: OriginatedData): OriginatedSeqData = from(data, mkComputedOrigin(data, source))
  def from(data: SomeData, source: OriginatedData): OriginatedSomeData = from(data, mkComputedOrigin(data, source))
  def from(data: NoneData, source: OriginatedData): OriginatedNoneData = from(data, mkComputedOrigin(data, source))

  def from(data: OptionData, source: OriginatedData): OriginatedOptionData = data match {
    case data: SomeData => from(data, source)
    case data: NoneData => from(data, source)
  }

  def from(data: Data, source: OriginatedData): OriginatedData = data match {
    case data: PrimitiveData[t] => from(data, source)
    case data: TupleData => from(data, source)
    case data: StructData => from(data, source)
    case data: SeqData => from(data, source)
    case data: OptionData => from(data, source)
  }

  /**
   * Methods for constructing originated data with self origin.
   */
  private def mkOriginalOrigin(data: Data) = OriginalOrigin(SelfPointer(data.datatype))

  def fromSelf[T](data: PrimitiveData[T]): OriginatedPrimitiveData[T] = from(data, mkOriginalOrigin(data))
  def fromSelf(data: TupleData): OriginatedTupleData = from(data, mkOriginalOrigin(data))
  def fromSelf(data: StructData): OriginatedStructData = from(data, mkOriginalOrigin(data))
  def fromSelf(data: SeqData): OriginatedSeqData = from(data, mkOriginalOrigin(data))
  def fromSelf(data: SomeData): OriginatedSomeData = from(data, mkOriginalOrigin(data))
  def fromSelf(data: NoneData): OriginatedNoneData = from(data, mkOriginalOrigin(data))

  def fromSelf(data: OptionData): OriginatedOptionData = data match {
    case data: SomeData => fromSelf(data)
    case data: NoneData => fromSelf(data)
  }

  def fromSelf(data: Data): OriginatedData = data match {
    case data: PrimitiveData[t] => fromSelf(data)
    case data: TupleData => fromSelf(data)
    case data: StructData => fromSelf(data)
    case data: SeqData => fromSelf(data)
    case data: OptionData => fromSelf(data)
  }
}

abstract class OriginatedPrimitiveData[T] extends OriginatedData {
  def datatype: PrimitiveType[T] = data.datatype
  def data: PrimitiveData[T]
  def value: T = data.value

  private def state = (datatype, data)

  final override def equals(that: Any) = that match {
    case that: OriginatedPrimitiveData[T] => this.state == that.state
  }

  final override def hashCode() = state.hashCode()
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

abstract class OriginatedNoneData extends OriginatedOptionData {
  private def state = (datatype, data)

  final override def equals(that: Any) = that match {
    case that: OriginatedNoneData => this.state == that.state
    case _ => false
  }

  final override def hashCode() = state.hashCode()
}

object OriginatedNoneData {
  def simple(origin: Origin): OriginatedNoneData = {
    SimpleOriginatedNoneData(origin)
  }
}

abstract class OriginatedSomeData extends OriginatedOptionData {
  def datatype = data.datatype
  def data: SomeData

  def value: OriginatedData

  private def state = (datatype, data)

  final override def equals(that: Any) = that match {
    case that: OriginatedSomeData => this.state == that.state
    case _ => false
  }

  final override def hashCode() = state.hashCode()
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

  private def state = (datatype, coordinates)

  final override def equals(that: Any) = that match {
    case that: OriginatedTupleData => this.state == that.state
    case _ => false
  }

  final override def hashCode() = state.hashCode()
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

  private def state = (datatype, features)

  final override def equals(that: Any) = that match {
    case that: OriginatedStructData => this.state == that.state
    case _ => false
  }

  final override def hashCode() = state.hashCode()
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

  private def state = (datatype, elements)

  final override def equals(that: Any) = that match {
    case that: OriginatedSeqData => this.state == that.state
    case _ => false
  }

  final override def hashCode() = state.hashCode()
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

