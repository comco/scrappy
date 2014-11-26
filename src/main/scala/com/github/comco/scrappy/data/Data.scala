package com.github.comco.scrappy.data

import scala.language.implicitConversions
import scala.reflect.runtime.universe.TypeTag

import com.github.comco.scrappy.PrimitiveType
import com.github.comco.scrappy.SeqType
import com.github.comco.scrappy.Shape
import com.github.comco.scrappy.StructType
import com.github.comco.scrappy.TupleType
import com.github.comco.scrappy.Type
import com.github.comco.scrappy.data.simple.SimplePrimitiveData
import com.github.comco.scrappy.data.simple.SimpleSeqData
import com.github.comco.scrappy.data.simple.SimpleSomeData
import com.github.comco.scrappy.data.simple.SimpleStructData
import com.github.comco.scrappy.data.simple.SimpleTupleData

/**
 * Base class for scrappy Data.
 * Represents a piece of immutable, typed and interchangeable data.
 * This data is intended to be independent of the specific representation,
 * so the equals methods of the derivable subclasses are final.
 */
sealed abstract class Data[+Shape <: Shape.Any: TypeTag] {
  /**
   * The scrappy type of this data.
   */
  def datatype: Type[Shape]
}

object Data {
  type Any = Data[Shape.Any]
  type Nil = Data[Shape.Nil]

  type Primitive[+RawType] = Data[Shape.Primitive[RawType]]
  implicit def toPrimitiveData[RawType](data: Primitive[RawType]) =
    data.asInstanceOf[PrimitiveData[RawType]]

  type Struct = Data[Shape.Struct]
  implicit def toStructData(data: Struct) = data.asInstanceOf[StructData]

  type Tuple = Data[Shape.Tuple]
  implicit def toTupleData(data: Tuple) = data.asInstanceOf[TupleData]

  type Seq[+ElementShape <: Shape.Any] = Data[Shape.Seq[ElementShape]]
  implicit def toSeqData[ElementShape <: Shape.Any](data: Seq[ElementShape]) =
    data.asInstanceOf[SeqData[ElementShape]]

  type Optional[+ValueShape <: Shape.Concrete] = Data[Shape.Optional[ValueShape]]
  implicit def toOptionalData[ValueShape <: Shape.Concrete](data: Optional[ValueShape]) =
    data.asInstanceOf[OptionalData[ValueShape]]

  type Some[+ValueShape <: Shape.Concrete] = Data[Shape.Some[ValueShape]]
  implicit def toSomeData[ValueShape <: Shape.Concrete](data: Data[Shape.Some[ValueShape]]) =
    data.asInstanceOf[SomeData[ValueShape]]

  def isFilled(data: Any): Boolean = data match {
    case data: OptionalData[_] => data.hasValue
    case _ => true
  }
}

abstract class PrimitiveData[+RawType: TypeTag] extends Data[Shape.Primitive[RawType]] {
  /**
   * The raw value of this primitive data.
   */
  def raw: RawType

  private def state = (datatype, raw)

  final override def equals(that: Any) = that match {
    case that: PrimitiveData[RawType] => this.state == that.state
    case _ => false
  }

  final override def hashCode() = state.hashCode()
}

object PrimitiveData {
  implicit def apply[RawType: TypeTag](value: RawType)(
    implicit datatype: PrimitiveType[RawType]): PrimitiveData[RawType] =
    SimplePrimitiveData(value)
}

sealed abstract class OptionalData[+ValueShape <: Shape.Any: TypeTag] extends Data.Optional[ValueShape] {
  /**
   * True if this optional data has a value.
   */
  def hasValue: Boolean
}

final case object NoneData extends OptionalData[Shape.Nil] {
  final override def hasValue = false
}

abstract class SomeData[+ValueShape <: Shape.Concrete: TypeTag] extends OptionalData[ValueShape] {
  /**
   * SomeData always contains a value.
   */
  final override def hasValue = true

  /**
   * The value of this option data.
   */
  def value: Data[ValueShape]

  private def state = (datatype, value)

  final override def equals(that: Any) = that match {
    case that: SomeData[ValueShape] => this.state == that.state
    case _ => false
  }

  final override def hashCode() = state.hashCode()
}

object SomeData {
  def apply[ValueShape <: Shape.Concrete: TypeTag](value: Data[ValueShape]): SomeData[ValueShape] = {
    SimpleSomeData(value)
  }
}

abstract class TupleData extends Data.Tuple {
  require(datatype.length == coordinates.length,
    s"Invalid size of coordinates to construct a TupleData;" +
      " expected: ${datatype.length}, actual: ${coordinates.length}.")
  require(datatype.coordinateTypes.zip(coordinates).forall {
    case (datatype, data) => data.datatype == datatype
  }, "Data coordinates types don't match tuple datatypes for TupleData construction.")

  /**
   * The coordinates of this tuple data.
   */
  def coordinates: IndexedSeq[Data.Any]

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
  def coordinate(position: Int): Data.Any = {
    require(datatype.hasCoordinate(position),
      s"Coordinate position: $position is out of bounds for TupleType: $datatype")

    coordinates(position)
  }

  private def state = (datatype, coordinates)

  final override def equals(that: Any) = that match {
    case that: TupleData => this.state == that.state
    case _ => false
  }

  final override def hashCode() = state.hashCode()
}

object TupleData {
  def apply(datatype: TupleType, coordinates: IndexedSeq[Data.Any]): TupleData = {
    SimpleTupleData(datatype, coordinates)
  }

  def apply(coordinates: Data.Any*): TupleData = {
    val datatype = TupleType(coordinates.map(_.datatype).toIndexedSeq)
    TupleData(datatype, coordinates.toIndexedSeq)
  }
}

abstract class SeqData[+ElementShape <: Shape.Any: TypeTag] extends Data.Seq[ElementShape] {
  /**
   * The elements of this seq data.
   */
  def elements: Seq[Data[ElementShape]]

  /**
   * Checks if this struct data has a feature with some name.
   * Names of optional features which are not filled-in
   * are regarded as not occupied.
   */
  def isOccupied(index: Int): Boolean = {
    0 <= index && index < length && Data.isFilled(elements(index))
  }

  /**
   * Retrieves the element of this seq data at some index.
   */
  def element(index: Int): Data[ElementShape] = {
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
    case that: SeqData[ElementShape] => this.state == that.state
    case _ => false
  }

  final override def hashCode() = state.hashCode()
}

object SeqData {
  def apply[ElementShape <: Shape: TypeTag](firstElement: Data[ElementShape],
    nextElements: Data[ElementShape]*): SeqData[ElementShape] =
    SimpleSeqData(SeqType(firstElement.datatype), firstElement +: nextElements.toSeq)

  def apply[ElementShape <: Shape: TypeTag](datatype: Type.Seq[ElementShape],
    elements: scala.Seq[Data[ElementShape]]): SeqData[ElementShape] = {
    SimpleSeqData(datatype, elements)
  }
}

abstract class StructData extends Data.Struct {
  /**
   * The features of this struct data.
   */
  def features: Map[String, Data.Any]

  /**
   * Checks if this struct data has a feature with some name.
   * Names of optional features which are not filled-in
   * are regarded as not occupied.
   */
  def isOccupied(name: String): Boolean = {
    features.contains(name) && Data.isFilled(features(name))
  }

  /**
   * Retrieves the feature of this struct data with some name.
   */
  def feature(name: String): Data.Any = {
    require(datatype.hasFeature(name),
      s"StructData: $this doesn't contain a feature named: $name")

    features(name)
  }

  private def state = (datatype, features)

  final override def equals(that: Any) = that match {
    case that: StructData => this.state == that.state
    case _ => false
  }

  final override def hashCode() = state.hashCode()
}

object StructData {
  def apply(datatype: StructType, features: Map[String, Data.Any]): StructData =
    SimpleStructData(datatype, features)

  def apply(datatype: StructType, features: (String, Data.Any)*): StructData =
    StructData(datatype, features.toMap)
}