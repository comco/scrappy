package com.github.comco.scrappy.data

import scala.language.implicitConversions
import com.github.comco.scrappy.OptionType
import com.github.comco.scrappy.PrimitiveType
import com.github.comco.scrappy.SeqType
import com.github.comco.scrappy.StructType
import com.github.comco.scrappy.TupleType
import com.github.comco.scrappy.Type
import com.github.comco.scrappy.data.simple.SimpleNoneData
import com.github.comco.scrappy.data.simple.SimplePrimitiveData
import com.github.comco.scrappy.data.simple.SimpleSeqData
import com.github.comco.scrappy.data.simple.SimpleSomeData
import com.github.comco.scrappy.data.simple.SimpleStructData
import com.github.comco.scrappy.data.simple.SimpleTupleData
import com.github.comco.scrappy.OptionType
import com.github.comco.scrappy.data.simple.SimplePrimitiveData

/**
 * Base class for scrappy Data.
 * Represents a piece of immutable, typed and interchangable data.
 * This data is intended to be independent of the specific representation,
 * so the equals methods of the derivable subclasses are final.
 */
sealed abstract class Data[+Datatype <: Type[Any]] {
  /**
   * The type of this data.
   */
  def datatype: Datatype
}

object Data {
  implicit def Data_To_PrimitiveData[T](data: Data[PrimitiveType[T]]) = data.asInstanceOf[PrimitiveData[T]]
  implicit def Data_To_TupleData(data: Data[TupleType]) = data.asInstanceOf[TupleData]
  implicit def Data_To_StructData(data: Data[StructType]) = data.asInstanceOf[StructData]
  implicit def Data_To_SeqData(data: Data[SeqType]) = data.asInstanceOf[SeqData]
  implicit def Data_To_OptionData(data: Data[OptionType]) = data.asInstanceOf[OptionData]

  /**
   * Checks if data contains a value.
   * OptionData in case of none data doesn't contain a value.
   */
  def isFilled(data: Data[Type[Any]]): Boolean = data match {
    case data: OptionData => data.isSome
    case _ => true
  }

  /**
   * Checks if data can be assigned to a field of type datatype.
   * An option data field can be assigned by its corresponding value type.
   */
  def canAssign(datatype: Type[Any], data: Data[Type[Any]]): Boolean = {
    data.datatype == datatype ||
      (datatype.isInstanceOf[OptionType] &&
        datatype.asInstanceOf[OptionType].someType == data.datatype)
  }

  /**
   * Converts data to a directly assignable to datatype value.
   */
  def convert(datatype: Type[Any], data: Data[Type[Any]]): Data[Type[Any]] = {
    assert(canAssign(datatype, data))
    if (data.datatype == datatype) {
      data
    } else {
      SomeData(datatype.asInstanceOf[OptionType], data)
    }
  }
}

abstract class PrimitiveData[T] extends Data[PrimitiveType[T]] {
  /**
   * The raw value of this primitive data.
   */
  def value: T

  private def state = (datatype, value)

  final override def equals(that: Any) = that match {
    case that: PrimitiveData[T] => this.state == that.state
    case _ => false
  }

  final override def hashCode() = state.hashCode()
}

object PrimitiveData {
  implicit def apply[T](value: T)(
    implicit datatype: PrimitiveType[T]): PrimitiveData[T] =
    SimplePrimitiveData(value)
}

sealed abstract class OptionData extends Data[OptionType] {
  /**
   * True if this option data has some value.
   */
  def isSome: Boolean
}

abstract class NoneData extends OptionData {
  def isSome = false

  private def state = (datatype)

  final override def equals(that: Any) = that match {
    case that: NoneData => this.state == that.state
    case _ => false
  }

  final override def hashCode() = state.hashCode()
}

object NoneData {
  def apply(datatype: OptionType): NoneData =
    SimpleNoneData(datatype)
}

abstract class SomeData extends OptionData {
  def isSome = true

  /**
   * The value of this option data.
   */
  def value: Data[Type[Any]]

  private def state = (datatype, value)

  final override def equals(that: Any) = that match {
    case that: SomeData => this.state == that.state
    case _ => false
  }

  final override def hashCode() = state.hashCode()
}

object SomeData {
  def doApply(datatype: OptionType, value: Data[Type[Any]]): SomeData =
    SimpleSomeData(datatype, value)

  def apply(datatype: OptionType, value: Data[Type[Any]]): SomeData = {
    require(value.datatype == datatype.someType,
      s"SomeData of datatype: $datatype cannot be created with value of type: ${value.datatype}")
    doApply(datatype, value)
  }

  implicit def apply[T](value: T)(
    implicit datatype: PrimitiveType[T]): SomeData = {
    apply(OptionType(datatype), PrimitiveData(value))
  }
}

abstract class TupleData extends Data[TupleType] {
  require(datatype.length == coordinates.length,
    s"Invalid size of coordinates to construct a TupleData;" +
      " expected: ${datatype.length}, actual: ${coordinates.length}.")
  require(datatype.coordinateTypes.zip(coordinates).forall {
    case (datatype, data) => data.datatype == datatype
  }, "Data coordinates types don't match tuple datatypes for TupleData construction.")

  /**
   * The coordinates of this tuple data.
   */
  def coordinates: IndexedSeq[Data[Type[Any]]]

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
  def coordinate(position: Int): Data[Type[Any]] = {
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
  def apply(datatype: TupleType, coordinates: IndexedSeq[Data[Type[Any]]]): TupleData = {
    SimpleTupleData(datatype, coordinates)
  }

  def apply(datatype: TupleType)(coordinates: Data[Type[Any]]*): TupleData = {
    TupleData(datatype, coordinates.toIndexedSeq)
  }

  def apply(coordinates: Data[Type[Any]]*): TupleData = {
    val datatype = TupleType(coordinates.map(_.datatype).toIndexedSeq)
    TupleData(datatype, coordinates.toIndexedSeq)
  }
}

abstract class SeqData extends Data[SeqType] {
  /**
   * The elements of this seq data.
   */
  def elements: Seq[Data[Type[Any]]]

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
  def element(index: Int): Data[Type[Any]] = {
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
    case that: SeqData => this.state == that.state
    case _ => false
  }

  final override def hashCode() = state.hashCode()
}

object SeqData {
  def doApply(datatype: SeqType, elements: Seq[Data[Type[Any]]]): SeqData =
    SimpleSeqData(datatype, elements)

  def apply(datatype: SeqType)(elements: Data[Type[Any]]*)(
    implicit dummy: DummyImplicit): SeqData = {
    SeqData(datatype, elements)
  }

  def apply(firstElement: Data[Type[Any]], nextElements: Data[Type[Any]]*): SeqData = {
    SeqData(SeqType(firstElement.datatype), firstElement +: nextElements.toSeq)
  }

  def apply(datatype: SeqType, rawElements: Seq[Data[Type[Any]]]): SeqData = {
    require(rawElements.forall(Data.canAssign(datatype.elementType, _)),
      "An element has invalid datatype")
    val elements = rawElements.map(Data.convert(datatype.elementType, _))
    doApply(datatype, elements)
  }
}

abstract class StructData extends Data[StructType] {
  /**
   * The features of this struct data.
   */
  def features: Map[String, Data[Type[Any]]]

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
  def feature(name: String): Data[Type[Any]] = {
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
  def doApply(datatype: StructType, features: Map[String, Data[Type[Any]]]): StructData =
    SimpleStructData(datatype, features)

  def apply(datatype: StructType, rawFeatures: Map[String, Data[Type[Any]]]): StructData = {
    require(rawFeatures.keys.forall(datatype.hasFeature(_)), "Invalid feature name")
    require(rawFeatures.forall {
      case (name, data) => {
        Data.canAssign(datatype.featureType(name), data)
      }
    }, s"Invalid feature type for creating a StructData with datatype: $datatype from features: $rawFeatures")
    require(datatype.featureTypes.forall {
      case (name, datatype) => rawFeatures.contains(name) || datatype.isInstanceOf[OptionType]
    }, "A non-optional feature is not given")

    val features: Map[String, Data[Type[Any]]] = {
      val convertedFeatures: Map[String, Data[Type[Any]]] = rawFeatures.transform({
        case (name, data) => Data.convert(datatype.featureType(name), data)
      })
      val missingFeatures: Map[String, Type[Any]] = datatype.featureTypes.filter({
        case (name, _) => !rawFeatures.contains(name)
      })
      // all missing features should have option types
      convertedFeatures ++ missingFeatures.map {
        case (name, datatype) => (name, NoneData(datatype.asInstanceOf[OptionType]))
      }
    }
    doApply(datatype, features)
  }

  def apply(datatype: StructType)(features: (String, Data[Type[Any]])*): StructData = {
    StructData(datatype, features.toMap)
  }
}