package com.github.comco.scrappy.originated_data

import scala.language.implicitConversions
import scala.reflect.runtime.universe.TypeTag
import com.github.comco.scrappy.PrimitiveType
import com.github.comco.scrappy.SeqType
import com.github.comco.scrappy.StructType
import com.github.comco.scrappy.TupleType
import com.github.comco.scrappy.Type
import com.github.comco.scrappy.data.Data
import com.github.comco.scrappy.data.NoneData
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
import com.github.comco.scrappy.originated_data.simple.SimpleOriginalSomeData
import com.github.comco.scrappy.Shape

/**
 * Base class for scrappy originated data - a piece of data with an origin.
 */
sealed abstract class OriginatedData[+Shape <: Shape.Any] {
  require(datatype == origin.targetType,
    s"Datatype of this originated data: $datatype and the targetType of the origin: $origin does not match.")

  /**
   * The type of this originated data.
   */
  def datatype: Type[Shape] = data.datatype

  def data: Data[Shape]
  def origin: Origin
}

object OriginatedData {
  type Any = OriginatedData[Shape.Any]
  type Nil = OriginatedData[Shape.Nil]

  type Primitive[T] = OriginatedData[Shape.Primitive[T]]
  implicit def toOriginatedPrimitiveData[T](data: Primitive[T]) =
    data.asInstanceOf[OriginatedPrimitiveData[T]]

  type Struct = OriginatedData[Shape.Struct]
  implicit def toOriginatedStructData(data: Struct) =
    data.asInstanceOf[OriginatedStructData]

  type Tuple = OriginatedData[Shape.Tuple]
  implicit def OriginatedData_To_OriginatedTupleData(data: Tuple) =
    data.asInstanceOf[OriginatedTupleData]

  type Seq[+ElementShape <: Shape.Any] = OriginatedData[Shape.Seq[ElementShape]]
  implicit def toOriginatedSeqData[ElementShape <: Shape.Any: TypeTag](data: Seq[ElementShape]) =
    data.asInstanceOf[OriginatedSeqData[ElementShape]]

  type Optional[+ValueShape <: Shape.Concrete] = OriginatedData[Shape.Optional[ValueShape]]
  implicit def toOriginatedOptionalData[ValueShape <: Shape.Concrete: TypeTag](data: Optional[ValueShape]) =
    data.asInstanceOf[OriginatedOptionalData[ValueShape]]

  type Some[+ValueShape <: Shape.Concrete] = OriginatedData[Shape.Some[ValueShape]]
  implicit def toOriginatedSomeData[ValueShape <: Shape.Concrete: TypeTag](data: Some[ValueShape]) =
    data.asInstanceOf[OriginatedSomeData[ValueShape]]

  /**
   * Methods for constructing originated data.
   */
  def from[Shape <: Shape.Any](data: Data[Shape], origin: Origin): OriginatedData[Shape] = ???
  def from[RawType: TypeTag](data: Data.Primitive[RawType], origin: Origin) = OriginatedPrimitiveData[RawType](data, origin)
  def from(data: Data.Tuple, origin: Origin) = OriginatedTupleData.original(data, origin)
  def from(data: Data.Struct, origin: Origin) = OriginatedStructData.original(data, origin)
  def from[ElementShape <: Shape.Any](data: Data.Seq[ElementShape], origin: Origin) = OriginatedSeqData.original(data, origin)
  def from[ValueShape <: Shape.Concrete](data: Data.Optional[ValueShape], origin: Origin): OriginatedData.Optional[ValueShape] = ???

  /**
   * Methods for constructing originated data by computing the target type.
   */
  private def mkComputedOrigin(data: Data.Any, source: OriginatedData.Any): ComputedOrigin = {
    source.origin.computedWithTargetType(data.datatype)
  }

  /**
   * Methods for constructing originated data with self origin.
   */
  private def mkOriginalOrigin(data: Data.Any) = OriginalOrigin(SelfPointer(data.datatype))

  def fromSelf[RawType: TypeTag](data: Data.Primitive[RawType]): OriginatedPrimitiveData[RawType] = from(data, mkOriginalOrigin(data))
  def fromSelf(data: Data.Tuple): OriginatedTupleData = from(data, mkOriginalOrigin(data))
  def fromSelf(data: Data.Struct): OriginatedStructData = from(data, mkOriginalOrigin(data))
  def fromSelf[ElementShape <: Shape.Any](data: Data.Seq[ElementShape]): OriginatedSeqData[ElementShape] = from(data, mkOriginalOrigin(data))

  def isFilled(originatedData: Any): Boolean = Data.isFilled(originatedData.data)
}

abstract class OriginatedPrimitiveData[T: TypeTag] extends OriginatedData.Primitive[T] {
  def raw = data.raw

  private def state = (datatype, data)

  final override def equals(that: Any) = that match {
    case that: OriginatedPrimitiveData[T] => this.state == that.state
    case _ => false
  }

  final override def hashCode() = state.hashCode()
}

object OriginatedPrimitiveData {
  def apply[T: TypeTag](data: PrimitiveData[T], origin: Origin): OriginatedPrimitiveData[T] = {
    SimpleOriginatedPrimitiveData(data, origin)
  }
}

sealed abstract class OriginatedOptionalData[+ValueShape <: Shape.Concrete]
    extends OriginatedData[Shape.Optional[ValueShape]] {
  def hasValue: Boolean
}

abstract class OriginatedNoneData extends OriginatedOptionalData[Shape.Nil] {
  final def hasValue = false

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

abstract class OriginatedSomeData[+ValueShape <: Shape.Concrete] extends OriginatedOptionalData[ValueShape] {
  final def hasValue = true

  def data: SomeData[ValueShape]

  def value: OriginatedData[ValueShape]

  private def state = (datatype, data)

  final override def equals(that: Any) = that match {
    case that: OriginatedSomeData[ValueShape] => this.state == that.state
    case _ => false
  }

  final override def hashCode() = state.hashCode()
}

object OriginatedSomeData {
  def original[ValueShape <: Shape.Concrete](data: SomeData[ValueShape], origin: Origin): OriginatedSomeData[ValueShape] = {
    SimpleOriginalSomeData(data, origin)
  }

  def computed[ValueShape <: Shape.Concrete](data: SomeData[ValueShape], origin: Origin, value: OriginatedData[ValueShape]): OriginatedSomeData[ValueShape] = {
    SimpleComputedSomeData(data, origin, value)
  }

  def computed[ValueShape <: Shape.Concrete: TypeTag](origin: Origin,
    value: OriginatedData[ValueShape]): OriginatedSomeData[ValueShape] = {
    val data = SomeData(value.data)
    computed(data, origin, value)
  }
}

abstract class OriginatedTupleData extends OriginatedData[Shape.Tuple] {
  /**
   * The coordinates of this tuple data.
   */
  def coordinates: IndexedSeq[OriginatedData.Any]

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
  def coordinate(position: Int): OriginatedData[Shape.Any] = {
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
    coordinates: IndexedSeq[OriginatedData.Any]): OriginatedTupleData =
    SimpleComputedTupleData(data, origin, coordinates)

  def apply(data: TupleData, origin: Origin): OriginatedTupleData =
    original(data, origin)

  def apply(data: TupleData,
    origin: Origin,
    coordinates: IndexedSeq[OriginatedData.Any]): OriginatedTupleData =
    computed(data, origin, coordinates)

  def apply(datatype: TupleType,
    origin: Origin,
    coordinates: IndexedSeq[OriginatedData.Any]): OriginatedTupleData = {
    val data = TupleData(datatype, coordinates map (_.data))
    computed(data, origin, coordinates)
  }

  def unapply(that: OriginatedTupleData) =
    Some((that.data, that.origin, that.coordinates))
}

abstract class OriginatedStructData extends OriginatedData.Struct {
  /**
   * The features of this originated struct data.
   */
  def features: Map[String, OriginatedData.Any]

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
  def feature(name: String): OriginatedData.Any = {
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
    features: Map[String, OriginatedData.Any]): OriginatedStructData =
    SimpleComputedStructData(data, origin, features)

  def apply(data: StructData, origin: Origin): OriginatedStructData =
    original(data, origin)

  def apply(data: StructData,
    origin: Origin,
    features: Map[String, OriginatedData.Any]): OriginatedStructData =
    computed(data, origin, features)

  def apply(datatype: StructType,
    origin: Origin,
    features: Map[String, OriginatedData.Any]): OriginatedStructData = {
    val data = StructData(datatype, features map {
      case (name, originatedData) => (name, originatedData.data)
    })
    computed(data, origin, features)
  }
}

abstract class OriginatedSeqData[+ValueShape <: Shape.Any] extends OriginatedData[Shape.Seq[ValueShape]] {
  /**
   * The elements of this originated seq data.
   */
  def elements: Seq[OriginatedData[ValueShape]]

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
  def element(index: Int): OriginatedData[ValueShape] = {
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
    case that: OriginatedSeqData[ValueShape] => this.state == that.state
    case _ => false
  }

  final override def hashCode() = state.hashCode()
}

object OriginatedSeqData {
  def original[ElementShape <: Shape.Any](
    data: Data.Seq[ElementShape],
    origin: Origin): OriginatedSeqData[ElementShape] =
    SimpleOriginalSeqData(data, origin)

  def computed[ElementShape <: Shape.Any](
    data: Data.Seq[ElementShape],
    origin: Origin,
    elements: Seq[OriginatedData[ElementShape]]): OriginatedData.Seq[ElementShape] =
    SimpleComputedSeqData(data, origin, elements)
}

